#include <stdio.h>

struct options_t
{
  bool verbose;
  bool read_stdin;
  FILE* current_output_device;
  FILE* current_input_device;
  const char* current_filename;
  const char* include_path;
};

extern options_t global_opts;

void set_default(struct options_t*);
void reset_for_programs(struct options_t*);
bool parse_option(const char* arg, struct options_t*);
void help();
void version();
