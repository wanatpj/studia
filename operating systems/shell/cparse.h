typedef struct command {
	char** argv;
	char * in_file_name;
	char * out_file_name;
	int append_mode;
} command_s;  


command_s* split_commands(char *);

int in_background(char*);

void print_command(command_s *);

