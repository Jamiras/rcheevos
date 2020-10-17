#include "internal.h"

#include <string.h>

static void rc_update_condition_pause(rc_condition_t* condition, int* in_pause) {
  if (condition->next != 0) {
    rc_update_condition_pause(condition->next, in_pause);
  }

  switch (condition->type) {
    case RC_CONDITION_PAUSE_IF:
      *in_pause = condition->pause = 1;
      break;

    case RC_CONDITION_ADD_SOURCE:
    case RC_CONDITION_SUB_SOURCE:
    case RC_CONDITION_ADD_HITS:
    case RC_CONDITION_AND_NEXT:
    case RC_CONDITION_OR_NEXT:
    case RC_CONDITION_ADD_ADDRESS:
      condition->pause = *in_pause;
      break;

    default:
      *in_pause = condition->pause = 0;
      break;
  }
}

static rc_condition_t* rc_parse_addaddress_chain(const char** memaddr, rc_parse_state_t* parse) {
  char variable_expr[256]; /* this should support ~16 levels of indirection */
  const char* ptr = *memaddr;
  const char* cond_start;
  size_t length;
  rc_operand_t operand;
  rc_condition_t* cond;
  int result;

  do {
    cond_start = ptr;
    ptr += 2; /* skip over 'I:' */

    result = rc_parse_operand(&operand, &ptr, 0, -1, parse);
    if (result < 0) {
      parse->offset = result;
      return 0;
    }

    if (rc_parse_operator(&ptr) != RC_OPERATOR_NONE) {
      result = rc_parse_operand(&operand, &ptr, 0, -1, parse);
      if (result < 0) {
        parse->offset = result;
        return 0;
      }
    }

    if (*ptr != '_')
      return 0;
    ++ptr;

  } while (ptr[1] == ':' && (ptr[0] == 'I' || ptr[0] == 'i'));

  cond_start = ptr;

  length = (size_t)(ptr - *memaddr);
  if (length > sizeof(variable_expr) - 16) {
    parse->offset = RC_INVALID_STATE;
    return 0;
  }

  memcpy(variable_expr, *memaddr, length);
  variable_expr[length++] = 'M';
  variable_expr[length++] = ':';

  /* process the next condition normally, then replace the first operand with the variable */
  *memaddr = cond_start;
  cond = rc_parse_condition(memaddr, parse, 0);
  if (cond) {
    if (cond->operand1.type != RC_OPERAND_CONST) {
      if (cond_start[1] == ':')
        cond_start += 2;

      rc_parse_operand(&operand, &ptr, 0, 0, parse);

      memcpy(&variable_expr[length], cond_start, (size_t)(ptr - cond_start));
      length += (size_t)(ptr - cond_start);
      variable_expr[length] = '\0';

      cond->operand1.type = RC_OPERAND_VARIABLE;
      cond->operand1.value.memref = rc_alloc_helper_variable(variable_expr, length, parse);
      if (parse->offset < 0)
        return 0;
    }

    if (cond->operand2.type != RC_OPERAND_CONST) {
      length -= (size_t)(ptr - cond_start);
      rc_parse_operator(&ptr);

      cond_start = ptr;
      rc_parse_operand(&operand, &ptr, 0, 0, parse);

      memcpy(&variable_expr[length], cond_start, (size_t)(ptr - cond_start));
      length += (size_t)(ptr - cond_start);
      variable_expr[length] = '\0';

      cond->operand2.type = RC_OPERAND_VARIABLE;
      cond->operand2.value.memref = rc_alloc_helper_variable(variable_expr, length, parse);
      if (parse->offset < 0)
        return 0;
    }
  }

  return cond;
}

rc_condset_t* rc_parse_condset(const char** memaddr, rc_parse_state_t* parse, int is_value) {
  rc_condset_t* self;
  rc_condition_t** next;
  int in_pause;
  int in_add_address;
  unsigned measured_target = 0;

  self = RC_ALLOC(rc_condset_t, parse);
  self->has_pause = self->is_paused = 0;
  next = &self->conditions;

  if (**memaddr == 'S' || **memaddr == 's' || !**memaddr) {
    /* empty group - editor allows it, so we have to support it */
    *next = 0;
    return self;
  }

  in_add_address = 0;
  for (;;) {
    if (parse->variables && (**memaddr == 'I' || **memaddr == 'i'))
      *next = rc_parse_addaddress_chain(memaddr, parse);
    else
      *next = rc_parse_condition(memaddr, parse, in_add_address);

    if (parse->offset < 0) {
      return 0;
    }

    if ((*next)->oper == RC_OPERATOR_NONE) {
      switch ((*next)->type) {
        case RC_CONDITION_ADD_ADDRESS:
        case RC_CONDITION_ADD_HITS:
        case RC_CONDITION_ADD_SOURCE:
        case RC_CONDITION_SUB_SOURCE:
        case RC_CONDITION_AND_NEXT:
        case RC_CONDITION_OR_NEXT:
          break;

        case RC_CONDITION_MEASURED:
          if (is_value)
            break;
          /* fallthrough to default */

        default:
          parse->offset = RC_INVALID_OPERATOR;
          return 0;
      }
    }

    self->has_pause |= (*next)->type == RC_CONDITION_PAUSE_IF;
    in_add_address = (*next)->type == RC_CONDITION_ADD_ADDRESS;

    switch ((*next)->type) {
    case RC_CONDITION_MEASURED:
      if (measured_target != 0) {
        /* multiple Measured flags cannot exist in the same group */
        parse->offset = RC_MULTIPLE_MEASURED;
        return 0;
      }
      else if (is_value) {
        measured_target = (unsigned)-1;
        if ((*next)->oper != RC_OPERATOR_NONE)
          (*next)->required_hits = measured_target;
      }
      else if ((*next)->required_hits != 0) {
        measured_target = (*next)->required_hits;
      }
      else if ((*next)->operand2.type == RC_OPERAND_CONST) {
        measured_target = (*next)->operand2.value.num;
      }
      else {
        parse->offset = RC_INVALID_MEASURED_TARGET;
        return 0;
      }

      if (parse->measured_target && measured_target != parse->measured_target) {
        /* multiple Measured flags in separate groups must have the same target */
        parse->offset = RC_MULTIPLE_MEASURED;
        return 0;
      }

      parse->measured_target = measured_target;
      break;

    case RC_CONDITION_STANDARD:
    case RC_CONDITION_TRIGGER:
      /* these flags are not allowed in value expressions */
      if (is_value) {
        parse->offset = RC_INVALID_VALUE_FLAG;
        return 0;
      }
      break;

    default:
      break;
    }

    next = &(*next)->next;

    if (**memaddr != '_') {
      break;
    }

    (*memaddr)++;
  }

  *next = 0;

  if (parse->buffer != 0) {
    in_pause = 0;
    rc_update_condition_pause(self->conditions, &in_pause);
  }

  return self;
}

static int rc_test_condset_internal(rc_condset_t* self, int processing_pause, rc_eval_state_t* eval_state) {
  rc_condition_t* condition;
  int set_valid, cond_valid, and_next, or_next;
  unsigned measured_value = 0;
  unsigned total_hits = 0;
  int can_measure = 1, measured_from_hits = 0;

  eval_state->primed = 1;
  set_valid = 1;
  and_next = 1;
  or_next = 0;
  eval_state->add_value = eval_state->add_hits = eval_state->add_address = 0;

  for (condition = self->conditions; condition != 0; condition = condition->next) {
    if (condition->pause != processing_pause) {
      continue;
    }

    /* STEP 1: process modifier conditions */
    switch (condition->type) {
      case RC_CONDITION_ADD_SOURCE:
        eval_state->add_value += rc_evaluate_condition_value(condition, eval_state);
        eval_state->add_address = 0;
        continue;

      case RC_CONDITION_SUB_SOURCE:
        eval_state->add_value -= rc_evaluate_condition_value(condition, eval_state);
        eval_state->add_address = 0;
        continue;

      case RC_CONDITION_ADD_ADDRESS:
        eval_state->add_address = rc_evaluate_condition_value(condition, eval_state);
        continue;

      case RC_CONDITION_MEASURED:
        if (condition->required_hits == 0) {
          /* Measured condition without a hit target measures the value of the left operand */
          measured_value = rc_evaluate_condition_value(condition, eval_state) + eval_state->add_value;
        }
        break;

      default:
        break;
    }

    /* STEP 2: evaluate the current condition */
    condition->is_true = rc_test_condition(condition, eval_state);
    eval_state->add_value = 0;
    eval_state->add_address = 0;

    /* apply logic flags and reset them for the next condition */
    cond_valid = condition->is_true;
    cond_valid &= and_next;
    cond_valid |= or_next;
    and_next = 1;
    or_next = 0;

    /* true conditions should update hit count */
    if (cond_valid) {
      eval_state->has_hits = 1;

      if (condition->required_hits == 0) {
        /* no target hit count, just keep tallying */
        ++condition->current_hits;
      }
      else if (condition->current_hits < condition->required_hits) {
        /* target hit count hasn't been met, tally and revalidate - only true if hit count becomes met */
        ++condition->current_hits;
        cond_valid = (condition->current_hits == condition->required_hits);
      }
      else {
        /* target hit count has been met, do nothing */
      }
    }
    else if (condition->current_hits > 0) {
      /* target has been true in the past, if the hit target is met, consider it true now */
      eval_state->has_hits = 1;
      cond_valid = (condition->current_hits == condition->required_hits);
    }

    /* STEP 3: handle logic flags */
    switch (condition->type) {
      case RC_CONDITION_ADD_HITS:
        eval_state->add_hits += condition->current_hits;
        continue;

      case RC_CONDITION_AND_NEXT:
        and_next = cond_valid;
        continue;

      case RC_CONDITION_OR_NEXT:
        or_next = cond_valid;
        continue;

      default:
        break;
    }

    /* STEP 4: calculate total hits */
    total_hits = condition->current_hits;

    if (eval_state->add_hits) {
      if (condition->required_hits != 0) {
        /* if the condition has a target hit count, we have to recalculate cond_valid including the AddHits counter */
        total_hits = condition->current_hits + eval_state->add_hits;
        cond_valid = (total_hits >= condition->required_hits);
      }
      else {
        /* no target hit count. we can't tell if the add_hits value is from this frame or not, so ignore it.
           complex condition will only be true if the current condition is true */
      }

      eval_state->add_hits = 0;
    }

    /* STEP 5: handle special flags */
    switch (condition->type) {
      case RC_CONDITION_PAUSE_IF:
        /* as soon as we find a PauseIf that evaluates to true, stop processing the rest of the group */
        if (cond_valid) {
          return 1;
        }

        /* if we make it to the end of the function, make sure we indicate that nothing matched. if we do find
           a later PauseIf match, it'll automatically return true via the previous condition. */
        set_valid = 0;

        if (condition->required_hits == 0) {
          /* PauseIf didn't evaluate true, and doesn't have a HitCount, reset the HitCount to indicate the condition didn't match */
          condition->current_hits = 0;
        }
        else {
          /* PauseIf has a HitCount that hasn't been met, ignore it for now. */
        }

        continue;

      case RC_CONDITION_RESET_IF:
        if (cond_valid) {
          eval_state->was_reset = 1; /* let caller know to reset all hit counts */
          set_valid = 0; /* cannot be valid if we've hit a reset condition */
        }
        continue;

      case RC_CONDITION_MEASURED:
        if (condition->required_hits != 0) {
          /* if there's a hit target, capture the current hits for recording Measured value later */
          measured_from_hits = 1;
          measured_value = total_hits;
        }
        break;

      case RC_CONDITION_MEASURED_IF:
        if (!cond_valid)
          can_measure = 0;
        break;

      case RC_CONDITION_TRIGGER:
        /* update truthiness of set, but do not update truthiness of primed state */
        set_valid &= cond_valid;
        continue;

      default:
        break;
    }

    /* STEP 5: update overall truthiness of set and primed state */
    eval_state->primed &= cond_valid;
    set_valid &= cond_valid;
  }

  /* if not suppressed, update the measured value */
  if (measured_value > eval_state->measured_value && can_measure) {
    eval_state->measured_value = measured_value;
    eval_state->measured_from_hits = measured_from_hits;
  }

  return set_valid;
}

int rc_test_condset(rc_condset_t* self, rc_eval_state_t* eval_state) {
  if (self->conditions == 0) {
    /* important: empty group must evaluate true */
    return 1;
  }

  if (self->has_pause) {
    if ((self->is_paused = rc_test_condset_internal(self, 1, eval_state))) {
      /* one or more Pause conditions exists, if any of them are true, stop processing this group */
      eval_state->primed = 0;
      return 0;
    }
  }

  return rc_test_condset_internal(self, 0, eval_state);
}

void rc_reset_condset(rc_condset_t* self) {
  rc_condition_t* condition;

  for (condition = self->conditions; condition != 0; condition = condition->next) {
    condition->current_hits = 0;
  }
}
