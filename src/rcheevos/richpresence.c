#include "internal.h"

#include "compat.h"

#include <ctype.h>

/* special formats only used by rc_richpresence_display_part_t.display_type. must not overlap other RC_FORMAT values */
enum {
  RC_FORMAT_STRING = 101,
  RC_FORMAT_LOOKUP = 102,
  RC_FORMAT_UNKNOWN_MACRO = 103
};

static const char* rc_parse_line(const char* line, const char** end) {
  const char* nextline;
  const char* endline;

  /* get a single line */
  nextline = line;
  while (*nextline && *nextline != '\n')
    ++nextline;

  /* find a trailing comment marker (//) */
  endline = line;
  while (endline < nextline && (endline[0] != '/' || endline[1] != '/' || (endline > line && endline[-1] == '\\')))
    ++endline;

  /* remove trailing whitespace */
  if (endline == nextline) {
    if (endline > line && endline[-1] == '\r')
      --endline;
  } else {
    while (endline > line && isspace(endline[-1]))
      --endline;
  }

  /* end is pointing at the first character to ignore - makes subtraction for length easier */
  *end = endline;

  if (*nextline == '\n')
    ++nextline;
  return nextline;
}

static rc_richpresence_display_t* rc_parse_richpresence_display_internal(const char* line, const char* endline, rc_parse_state_t* parse, rc_richpresence_t* richpresence) {
  rc_richpresence_display_t* self;
  rc_richpresence_display_part_t* part;
  rc_richpresence_display_part_t** next;
  rc_richpresence_lookup_t* lookup;
  const char* ptr;
  const char* in;
  char* out;

  if (endline - line < 1) {
    parse->offset = RC_MISSING_DISPLAY_STRING;
    return 0;
  }

  {
    self = RC_ALLOC(rc_richpresence_display_t, parse);
    memset(self, 0, sizeof(rc_richpresence_display_t));
    next = &self->display;
  }

  /* break the string up on macros: text @macro() moretext */
  do {
    ptr = line;
    while (ptr < endline) {
      if (*ptr == '@' && (ptr == line || ptr[-1] != '\\')) /* ignore escaped @s */
        break;

      ++ptr;
    }

    if (ptr > line) {
      part = RC_ALLOC(rc_richpresence_display_part_t, parse);
      memset(part, 0, sizeof(rc_richpresence_display_part_t));
      *next = part;
      next = &part->next;

      /* handle string part */
      part->display_type = RC_FORMAT_STRING;
      part->text = rc_alloc_str(parse, line, (int)(ptr - line));
      if (part->text) {
        /* remove backslashes used for escaping */
        in = part->text;
        while (*in && *in != '\\')
          ++in;

        if (*in == '\\') {
          out = (char*)in++;
          while (*in) {
            *out++ = *in++;
            if (*in == '\\')
              ++in;
          }
          *out = '\0';
        }
      }
    }

    if (*ptr == '@') {
      /* handle macro part */
      line = ++ptr;
      while (ptr < endline && *ptr != '(')
        ++ptr;

      if (ptr == endline) {
        parse->offset = RC_MISSING_VALUE;
        return 0;
      }

      if (ptr > line) {
        if (!parse->buffer) {
          /* just calculating size, can't confirm lookup exists */
          part = RC_ALLOC(rc_richpresence_display_part_t, parse);

          in = line;
          line = ++ptr;
          while (ptr < endline && *ptr != ')')
            ++ptr;
          if (*ptr == ')') {
            rc_parse_value_internal(&part->value, &line, parse);
            if (parse->offset < 0)
              return 0;
            ++ptr;
          } else {
            /* no closing parenthesis - allocate space for the invalid string */
            --in; /* already skipped over @ */
            rc_alloc_str(parse, line, (int)(ptr - in));
          }

        } else {
          /* find the lookup and hook it up */
          lookup = richpresence->first_lookup;
          while (lookup) {
            if (strncmp(lookup->name, line, ptr - line) == 0 && lookup->name[ptr - line] == '\0') {
              part = RC_ALLOC(rc_richpresence_display_part_t, parse);
              *next = part;
              next = &part->next;

              part->text = lookup->name;
              part->lookup = lookup;
              part->display_type = lookup->format;

              in = line;
              line = ++ptr;
              while (ptr < endline && *ptr != ')')
                ++ptr;
              if (*ptr == ')') {
                rc_parse_value_internal(&part->value, &line, parse);
                part->value.memrefs = 0;
                if (parse->offset < 0)
                  return 0;
                ++ptr;
              }
              else {
                /* non-terminated macro, dump the macro and the remaining portion of the line */
                --in; /* already skipped over @ */
                part->display_type = RC_FORMAT_STRING;
                part->text = rc_alloc_str(parse, in, (int)(ptr - in));
              }

              break;
            }

            lookup = lookup->next;
          }

          if (!lookup) {
            part = RC_ALLOC(rc_richpresence_display_part_t, parse);
            memset(part, 0, sizeof(rc_richpresence_display_part_t));
            *next = part;
            next = &part->next;

            /* find the closing parenthesis */
            while (ptr < endline && *ptr != ')')
              ++ptr;
            if (*ptr == ')')
              ++ptr;

            /* assert: the allocated string is going to be smaller than the memory used for the parameter of the macro */
            part->display_type = RC_FORMAT_UNKNOWN_MACRO;
            part->text = rc_alloc_str(parse, line, (int)(ptr - line));
          }
        }
      }
    }

    line = ptr;
  } while (line < endline);

  *next = 0;

  return self;
}

static void rc_insert_richpresence_lookup_item(rc_richpresence_lookup_t* lookup,
    unsigned first, unsigned last, const char* label, int label_len, rc_parse_state_t* parse)
{
  rc_richpresence_lookup_item_t** next;
  rc_richpresence_lookup_item_t* item;

  next = &lookup->root;
  while ((item = *next) != NULL) {
    if (first > item->last) {
      if (first == item->last + 1 &&
          strncmp(label, item->label, label_len) == 0 && item->label[label_len] == '\0') {
        item->last = last;
        return;
      }

      next = &item->right;
    }
    else if (last < item->first) {
      if (last == item->first - 1 &&
          strncmp(label, item->label, label_len) == 0 && item->label[label_len] == '\0') {
        item->first = first;
        return;
      }

      next = &item->left;
    }
    else {
      parse->offset = RC_DUPLICATED_VALUE;
      return;
    }
  }

  item = RC_ALLOC_SCRATCH(rc_richpresence_lookup_item_t, parse);
  item->first = first;
  item->last = last;
  item->label = rc_alloc_str(parse, label, label_len);
  item->left = item->right = NULL;

  *next = item;
}

static const char* rc_parse_richpresence_lookup(rc_richpresence_lookup_t* lookup, const char* nextline, rc_parse_state_t* parse)
{
  const char* line;
  const char* endline;
  const char* label;
  char* endptr = 0;
  unsigned first, last;
  int base;

  do
  {
    line = nextline;
    nextline = rc_parse_line(line, &endline);

    if (endline - line < 2) {
      /* ignore full line comments inside a lookup */
      if (line[0] == '/' && line[1] == '/')
        continue;

      /* empty line indicates end of lookup */
      break;
    }

    /* "*=XXX" specifies default label if lookup does not provide a mapping for the value */
    if (line[0] == '*' && line[1] == '=') {
      line += 2;
      lookup->default_label = rc_alloc_str(parse, line, (int)(endline - line));
      continue;
    }

    label = line;
    while (label < endline && *label != '=')
      ++label;

    if (label == endline) {
      parse->offset = RC_MISSING_VALUE;
      break;
    }
    ++label;

    do {
      if (line[0] == '0' && line[1] == 'x') {
        line += 2;
        base = 16;
      } else {
        base = 10;
      }

      first = strtoul(line, &endptr, base);
      if (*endptr != '-') {
        last = first;
      }
      else {
        line = endptr + 1;

        if (line[0] == '0' && line[1] == 'x') {
          line += 2;
          base = 16;
        } else {
          base = 10;
        }

        last = strtoul(line, &endptr, base);
      }

      if (*endptr == '=') {
        rc_insert_richpresence_lookup_item(lookup, first, last, label, (int)(endline - label), parse);
        break;
      }

      if (*endptr != ',') {
        parse->offset = RC_INVALID_CONST_OPERAND;
        break;
      }

      rc_insert_richpresence_lookup_item(lookup, first, last, label, (int)(endline - label), parse);
      line = endptr + 1;
    } while (line < endline);

  } while (parse->offset > 0);

  return nextline;
}

void rc_parse_richpresence_internal(rc_richpresence_t* self, const char* script, rc_parse_state_t* parse) {
  rc_richpresence_display_t** nextdisplay;
  rc_richpresence_lookup_t** nextlookup;
  rc_richpresence_lookup_t* lookup;
  rc_trigger_t* trigger;
  char format[64];
  const char* display = 0;
  const char* line;
  const char* nextline;
  const char* endline;
  const char* ptr;
  int hasdisplay = 0;
  int chars;

  nextlookup = &self->first_lookup;

  /* first pass: process macro initializers */
  line = script;
  while (*line)
  {
    nextline = rc_parse_line(line, &endline);
    if (strncmp(line, "Lookup:", 7) == 0) {
      line += 7;

      lookup = RC_ALLOC(rc_richpresence_lookup_t, parse);
      lookup->name = rc_alloc_str(parse, line, (int)(endline - line));
      lookup->format = RC_FORMAT_LOOKUP;
      lookup->root = NULL;
      lookup->default_label = "";
      *nextlookup = lookup;
      nextlookup = &lookup->next;

      nextline = rc_parse_richpresence_lookup(lookup, nextline, parse);
      if (parse->offset < 0)
        return;

    } else if (strncmp(line, "Format:", 7) == 0) {
      line += 7;

      lookup = RC_ALLOC(rc_richpresence_lookup_t, parse);
      lookup->name = rc_alloc_str(parse, line, (int)(endline - line));
      lookup->root = NULL;
      lookup->default_label = "";
      *nextlookup = lookup;
      nextlookup = &lookup->next;

      line = nextline;
      nextline = rc_parse_line(line, &endline);
      if (parse->buffer && strncmp(line, "FormatType=", 11) == 0) {
        line += 11;

        chars = (int)(endline - line);
        if (chars > 63)
          chars = 63;
        memcpy(format, line, chars);
        format[chars] = '\0';

        lookup->format = rc_parse_format(format);
      } else {
        lookup->format = RC_FORMAT_VALUE;
      }
    } else if (strncmp(line, "Display:", 8) == 0) {
      display = nextline;

      do {
        line = nextline;
        nextline = rc_parse_line(line, &endline);
      } while (*line == '?');
    }

    line = nextline;
  }

  *nextlookup = 0;
  nextdisplay = &self->first_display;

  /* second pass, process display string*/
  if (display) {
    line = display;
    nextline = rc_parse_line(line, &endline);

    while (*line == '?') {
      /* conditional display: ?trigger?string */
      ptr = ++line;
      while (ptr < endline && *ptr != '?')
        ++ptr;

      if (ptr < endline) {
        *nextdisplay = rc_parse_richpresence_display_internal(ptr + 1, endline, parse, self);
        if (parse->offset < 0)
          return;
        trigger = &((*nextdisplay)->trigger);
        rc_parse_trigger_internal(trigger, &line, parse);
        trigger->memrefs = 0;
        if (parse->offset < 0)
          return;
        if (parse->buffer)
          nextdisplay = &((*nextdisplay)->next);
      }

      line = nextline;
      nextline = rc_parse_line(line, &endline);
    }

    /* non-conditional display: string */
    *nextdisplay = rc_parse_richpresence_display_internal(line, endline, parse, self);
    if (*nextdisplay) {
      hasdisplay = 1;
      nextdisplay = &((*nextdisplay)->next);
    }
  }

  /* finalize */
  *nextdisplay = 0;

  if (!hasdisplay && parse->offset > 0) {
    parse->offset = RC_MISSING_DISPLAY_STRING;
  }
}

int rc_richpresence_size(const char* script) {
  rc_richpresence_t* self;
  rc_parse_state_t parse;
  rc_memref_value_t* first_memref;
  rc_init_parse_state(&parse, 0, 0, 0);
  rc_init_parse_state_memrefs(&parse, &first_memref);

  self = RC_ALLOC(rc_richpresence_t, &parse);
  rc_parse_richpresence_internal(self, script, &parse);

  rc_destroy_parse_state(&parse);
  return parse.offset;
}

rc_richpresence_t* rc_parse_richpresence(void* buffer, const char* script, lua_State* L, int funcs_ndx) {
  rc_richpresence_t* self;
  rc_parse_state_t parse;
  rc_init_parse_state(&parse, buffer, L, funcs_ndx);

  self = RC_ALLOC(rc_richpresence_t, &parse);
  rc_init_parse_state_memrefs(&parse, &self->memrefs);

  rc_parse_richpresence_internal(self, script, &parse);

  rc_destroy_parse_state(&parse);
  return parse.offset >= 0 ? self : 0;
}

int rc_evaluate_richpresence(rc_richpresence_t* richpresence, char* buffer, unsigned buffersize, rc_peek_t peek, void* peek_ud, lua_State* L) {
  rc_richpresence_display_t* display;
  rc_richpresence_display_part_t* part;
  rc_richpresence_lookup_item_t* item;
  char tmp[256];
  char* ptr;
  const char* text;
  size_t chars;
  unsigned value;

  rc_update_memref_values(richpresence->memrefs, peek, peek_ud);

  ptr = buffer;
  display = richpresence->first_display;
  while (display) {
    if (!display->next || rc_test_trigger(&display->trigger, peek, peek_ud, L)) {
      part = display->display;
      while (part) {
        switch (part->display_type) {
          case RC_FORMAT_STRING:
            text = part->text;
            chars = strlen(text);
            break;

          case RC_FORMAT_LOOKUP:
            value = rc_evaluate_value(&part->value, peek, peek_ud, L);
            text = part->lookup->default_label;
            item = part->lookup->root;
            while (item) {
              if (item->first > value) {
                item = item->left;
              } else if (item->last < value) {
                item = item->right;
              } else {
                text = item->label;
                break;
              }
            }

            chars = strlen(text);
            break;

          case RC_FORMAT_UNKNOWN_MACRO:
            chars = snprintf(tmp, sizeof(tmp), "[Unknown macro]%s", part->text);
            text = tmp;
            break;

          default:
            value = rc_evaluate_value(&part->value, peek, peek_ud, L);
            chars = rc_format_value(tmp, sizeof(tmp), value, part->display_type);
            text = tmp;
            break;
        }

        if (chars > 0 && buffersize > 0) {
          if ((unsigned)chars >= buffersize) {
            /* prevent write past end of buffer */
            memcpy(ptr, text, buffersize - 1);
            ptr[buffersize - 1] = '\0';
            buffersize = 0;
          }
          else {
            memcpy(ptr, text, chars);
            ptr[chars] = '\0';
            buffersize -= (unsigned)chars;
          }
        }

        ptr += chars;
        part = part->next;
      }

      return (int)(ptr - buffer);
    }

    display = display->next;
  }

  buffer[0] = '\0';
  return 0;
}
