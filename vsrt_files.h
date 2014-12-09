/*  adapted from phast_files.h file of PHAST program */
#ifndef _INC_VSRT_FILES_H
#define _INC_VSRT_FILES_H

#ifdef PHREEQC_IDENT
static char const svnid[] =
	"$Id: phast_files.h 4270 2010-04-20 20:44:19Z charlton $";
#endif

FILE *open_echo(const char *prefix);
int close_input_files(void);
int close_output_files(void);
int getc_callback(void *cookie);
int open_input_files_vsrt(char *chemistry_name, char *database_name,
						   void **db_cookie, void **input_cookie);
int open_output_file(char *prefix, int solute);
int open_punch_file(char *prefix, int solute);
int vsrt_handler(const int action, const int type, const char *err_str,
				  const int stop, void *cookie, const char *format,
				  va_list args);
int process_file_names(int argc, char *argv[], void **db_cookie,
					   void **input_cookie, int log);

#endif /* _INC_PHAST_FILES_H */
