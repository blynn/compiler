#include <stdio.h>
#include <stdlib.h>

// CONSTANT FALSE 0
#define FALSE 0
// CONSTANT TRUE 1
#define TRUE 1

int match(char* a, char* b);
void file_print(char* s, FILE* f);
void require(int bool, char* error);

FILE* source_file;
FILE* destination_file;

int scrub_line_comment()
{
	int c = fgetc(source_file);
	while(10 != c)
	{
		require(EOF != c, "hit EOF in line comment\nThis is not valid input\n");
		c = fgetc(source_file);
	}
	return c;
}

int preserve_string()
{
	int c = fgetc(source_file);
	int escape = FALSE;
	do
	{
		if(!escape && '\\' == c ) escape = TRUE;
		else escape = FALSE;

		if(!escape) fputc(c, destination_file);
		c = fgetc(source_file);
		if(escape && 'n' == c)
		{
			fputc('\n', destination_file);
			c = fgetc(source_file);
		}
		require(EOF != c, "Unterminated string\n");
	} while(escape || (c != '"'));
	return fgetc(source_file);
}


void process_file()
{
	int c;

	do
	{
		c = fgetc(source_file);
		if('"' == c) preserve_string();
		else if('#' == c) c = scrub_line_comment();
	} while(EOF != c);
}

int main(int argc, char **argv)
{
	source_file = stdin;
	destination_file = stdout;

	int option_index = 1;
	while(option_index <= argc)
	{
		if(NULL == argv[option_index])
		{
			option_index = option_index + 1;
		}
		else if(match(argv[option_index], "-o") || match(argv[option_index], "--output"))
		{
			destination_file = fopen(argv[option_index + 1], "w");

			if(NULL == destination_file)
			{
				file_print("The file: ", stderr);
				file_print(argv[option_index + 1], stderr);
				file_print(" can not be opened!\n", stderr);
				exit(EXIT_FAILURE);
			}
			option_index = option_index + 2;
		}
		else if(match(argv[option_index], "-f") || match(argv[option_index], "--file"))
		{
			source_file = fopen(argv[option_index + 1], "r");
			if(NULL == source_file)
			{
				file_print("The file: ", stderr);
				file_print(argv[option_index + 1], stderr);
				file_print(" can not be opened!\n", stderr);
				exit(EXIT_FAILURE);
			}
			option_index = option_index + 2;
		}
		else
		{
			file_print("bad command: ", stdout);
			file_print(argv[option_index], stdout);
			file_print("\n", stdout);
			exit(EXIT_FAILURE);
		}
	}

	process_file();
	exit(EXIT_SUCCESS);
}
