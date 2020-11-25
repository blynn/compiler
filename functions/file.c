/* Copyright (C) 2016 Jeremiah Orians
 * This file is part of M2-Planet.
 *
 * M2-Planet is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * M2-Planet is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with M2-Planet.  If not, see <http://www.gnu.org/licenses/>.
 */

// CONSTANT stdin 0
// CONSTANT stdout 1
// CONSTANT stderr 2
// CONSTANT EOF 0xFFFFFFFF

int fgetc(FILE* f)
{
	asm("LOAD_IMMEDIATE_eax %3"
	"LOAD_EFFECTIVE_ADDRESS_ebx %4"
	"LOAD_INTEGER_ebx"
	"PUSH_ebx"
	"COPY_esp_to_ecx"
	"LOAD_IMMEDIATE_edx %1"
	"INT_80"
	"TEST"
	"POP_eax"
	"JUMP_NE8 !FUNCTION_fgetc_Done"
	"LOAD_IMMEDIATE_eax %-1"
	":FUNCTION_fgetc_Done");
}

void fputc(char s, FILE* f)
{
	asm("LOAD_IMMEDIATE_eax %4"
	"LOAD_EFFECTIVE_ADDRESS_ebx %4"
	"LOAD_INTEGER_ebx"
	"LOAD_EFFECTIVE_ADDRESS_ecx %8"
	"LOAD_IMMEDIATE_edx %1"
	"INT_80");
}

/* Important values needed for open */
// CONSTANT O_RDONLY 0
// CONSTANT O_WRONLY 1
// CONSTANT O_RDWR 2
// CONSTANT O_CREAT 64
// CONSTANT O_TRUNC 512
/* 00700 in octal is 448*/
// CONSTANT S_IRWXU 448
/* 00100 in octal is 64 */
// CONSTANT S_IXUSR 64
/* 00200 in octal is 128 */
// CONSTANT S_IWUSR 128
/* 00400 in octal is 256 */
// CONSTANT S_IRUSR 256

FILE* open(char* name, int flag, int mode)
{
	asm("LOAD_EFFECTIVE_ADDRESS_ebx %12"
	"LOAD_INTEGER_ebx"
	"LOAD_EFFECTIVE_ADDRESS_ecx %8"
	"LOAD_INTEGER_ecx"
	"LOAD_EFFECTIVE_ADDRESS_edx %4"
	"LOAD_INTEGER_edx"
	"LOAD_IMMEDIATE_eax %5"
	"INT_80");
}

FILE* fopen(char* filename, char* mode)
{
	FILE* f;
	if('w' == mode[0])
	{ /* 577 is O_WRONLY|O_CREAT|O_TRUNC, 384 is 600 in octal */
		f = open(filename, 577 , 384);
	}
	else
	{ /* Everything else is a read */
		f = open(filename, 0, 0);
	}

	/* Negative numbers are error codes */
	if(0 > f)
	{
		return 0;
	}
	return f;
}

int close(int fd)
{
	asm("LOAD_EFFECTIVE_ADDRESS_ebx %4"
	"LOAD_IMMEDIATE_eax %6"
	"INT_80");
}
int fclose(FILE* stream)
{
	int error = close(stream);
	return error;
}

int fflush(FILE *stream){
	/* We don't buffer, nothing to flush */
	return 0;
}

// CONSTANT SEEK_SET 0
// CONSTANT SEEK_CUR 1
// CONSTANT SEEK_END 2

/* We just use lseek directly */
int fseek(FILE* f, long offset, int whence)
{
	asm("LOAD_EFFECTIVE_ADDRESS_ebx %12"
	"LOAD_INTEGER_ebx"
	"LOAD_EFFECTIVE_ADDRESS_ecx %8"
	"LOAD_INTEGER_ecx"
	"LOAD_EFFECTIVE_ADDRESS_edx %4"
	"LOAD_INTEGER_edx"
	"LOAD_IMMEDIATE_eax %19"
	"INT_80");
}

void rewind(FILE* f)
{
	fseek(f, 0, SEEK_SET);
}
