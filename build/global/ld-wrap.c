#define _GNU_SOURCE
#include <dlfcn.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

static int (*o_execv)(const char *filename, char *const argv[]) = NULL;
static int (*o_execve)(const char *filename, char *const argv[], char *const envp[]) = NULL;

char **ld_wrap_argv(const char *filename, char *const argv[]){
  char **argv_t = malloc(3*sizeof(char *));
  argv_t[0] = "--library-path";
  argv_t[1] = getenv("LW_LIBRARY_PATH");
  for(int i=0; argv[i]; ++i){
    argv_t[i+2] = argv[i];
  }
  return argv_t;
}

int execv(const char *filename, char *const argv[]){
  char **argv_t = ld_wrap_argv(filename, argv);
  o_execv = o_execv ? o_execv : dlsym(RTLD_NEXT, "execv");
  int status = o_execv(getenv("LW_LOADER_PATH"), argv_t);
  free(argv_t);
  return status;
}

int execve(const char *filename, char *const argv[], char *const envp[]){
  char **argv_t = ld_wrap_argv(filename, argv);
  o_execve = o_execve ? o_execve : dlsym(RTLD_NEXT, "execve");
  int status = o_execve(getenv("LW_LOADER_PATH"), argv_t, envp);
  free(argv_t);
  return status;
}
