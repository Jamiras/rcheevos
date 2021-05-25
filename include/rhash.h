#ifndef RHASH_H
#define RHASH_H

#include <stddef.h>
#include <stdio.h>
#include <stdint.h>

#include "rconsoles.h"

#ifdef __cplusplus
extern "C" {
#endif

  /* ===================================================== */

  /* generates a hash from a block of memory.
   * returns non-zero on success, or zero on failure.
   */
  int rc_hash_generate_from_buffer(char hash[33], int console_id, uint8_t* buffer, size_t buffer_size);

  /* generates a hash from a file.
   * returns non-zero on success, or zero on failure.
   */
  int rc_hash_generate_from_file(char hash[33], int console_id, const char* path);

  /* ===================================================== */

  /* data for rc_hash_iterate
   */
  struct rc_hash_iterator
  {
    uint8_t* buffer;
    size_t buffer_size;
    uint8_t consoles[12];
    int index;
    const char* path;
  };

  /* initializes a rc_hash_iterator
   * - path must be provided
   * - if buffer and buffer_size are provided, path may be a filename (i.e. for something extracted from a zip file)
   */
  void rc_hash_initialize_iterator(struct rc_hash_iterator* iterator, const char* path, uint8_t* buffer, size_t buffer_size);

  /* releases resources associated to a rc_hash_iterator
   */
  void rc_hash_destroy_iterator(struct rc_hash_iterator* iterator);

  /* generates the next hash for the data in the rc_hash_iterator.
   * returns non-zero if a hash was generated, or zero if no more hashes can be generated for the data.
   */
  int rc_hash_iterate(char hash[33], struct rc_hash_iterator* iterator);

  /* ===================================================== */

  /* specifies a function to call when an error occurs to display the error message */
  typedef void (*rc_hash_message_callback)(const char*);
  void rc_hash_init_error_message_callback(rc_hash_message_callback callback);

  /* specifies a function to call for verbose logging */
  void rc_hash_init_verbose_message_callback(rc_hash_message_callback callback);

  /* ===================================================== */

  /* opens a file */
  typedef void* (*rc_hash_filereader_open_file_handler)(const char* path_utf8);

  /* moves the file pointer - standard fseek parameters */
  typedef void (*rc_hash_filereader_seek_handler)(void* file_handle, size_t offset, int origin);

  /* locates the file pointer */
  typedef size_t (*rc_hash_filereader_tell_handler)(void* file_handle);

  /* reads the specified number of bytes from the file starting at the read pointer.
   * returns the number of bytes actually read.
   */
  typedef size_t (*rc_hash_filereader_read_handler)(void* file_handle, void* buffer, size_t requested_bytes);

  /* closes the file */
  typedef void (*rc_hash_filereader_close_file_handler)(void* file_handle);

  struct rc_hash_filereader
  {
    rc_hash_filereader_open_file_handler      open;
    rc_hash_filereader_seek_handler           seek;
    rc_hash_filereader_tell_handler           tell;
    rc_hash_filereader_read_handler           read;
    rc_hash_filereader_close_file_handler     close;
  };

  void rc_hash_init_custom_filereader(struct rc_hash_filereader* reader);

  /* ===================================================== */

  /* opens a track from the specified file. track 0 indicates the largest data track should be opened.
   * returns a handle to be passed to the other functions, or NULL if the track could not be opened.
   */
  typedef void* (*rc_hash_cdreader_open_track_handler)(const char* path, uint32_t track);

  /* attempts to read the specified number of bytes from the file starting at the read pointer.
   * returns the number of bytes actually read.
   */
  typedef size_t (*rc_hash_cdreader_read_sector_handler)(void* track_handle, uint32_t sector, void* buffer, size_t requested_bytes);

  /* closes the track handle */
  typedef void (*rc_hash_cdreader_close_track_handler)(void* track_handle);

  struct rc_hash_cdreader
  {
    rc_hash_cdreader_open_track_handler      open_track;
    rc_hash_cdreader_read_sector_handler     read_sector;
    rc_hash_cdreader_close_track_handler     close_track;
  };

  void rc_hash_init_default_cdreader();
  void rc_hash_init_custom_cdreader(struct rc_hash_cdreader* reader);

  /* ===================================================== */

#ifdef __cplusplus
}
#endif

#endif /* RHASH_H */