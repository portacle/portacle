#define _GNU_SOURCE
#include <dlfcn.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <sys/stat.h>
#include <errno.h>
#include <paths.h>

int (*o_execve)(const char *filename, char *const argv[], char *const envp[]) = NULL;
FILE *ld_wrap_logfile = NULL;

void init(){
  o_execve = dlsym(RTLD_NEXT, "execve");
}

// Returns a fresh array of strings to use as the proper argv
// to our ld-linux.so call.
char **ld_wrap_argv(const char *filename, char *const argv[]){
  int len;
  for(len=0; argv[len]; ++len);
  
  char **argv_t = calloc(4+len, sizeof(char *));
  argv_t[0] = "ld-linux.so";
  argv_t[1] = "--library-path";
  argv_t[2] = getenv("LW_LIBRARY_PATH");
  argv_t[3] = (char *)filename;
  for(int i=1; argv[i]; ++i){
    argv_t[i+3] = argv[i];
  }
  argv_t[len+3] = 0;
  return argv_t;
}

// Simply prints a debug message for the filename and args.
void ld_wrap_log(const char *filename, char *const argv[]){
  if(ld_wrap_logfile == NULL) ld_wrap_logfile = fopen("/tmp/ld-wrap.log", "w");
  fprintf(ld_wrap_logfile, "LD_WRAP: %s", filename);
  for(int i=1; argv[i]; ++i)
    fprintf(ld_wrap_logfile, " %s", argv[i]);
  fprintf(ld_wrap_logfile, "\n");
  fflush(ld_wrap_logfile);
}

// Returns a fresh copy of the search path to use.
char *ld_wrap_path(){
  char *path = getenv("PATH");
  if(path == NULL){
    size_t len = confstr(_CS_PATH, (char *)NULL, 0);
    char *path = calloc(2+len, sizeof(char));
    path[0] = ':';
    confstr(_CS_PATH, path+1, len);
    return path;
  }else{
    return strdup(path);
  }
}

// Returns true if the given filename exists, is a
// file and has the executable bit set.
int ld_wrap_exe_p(char *filename){
  struct stat st;
  return (stat(filename, &st) == 0)
    && S_ISREG(st.st_mode) 
    && (S_IXUSR & st.st_mode);
}

// Attempt to resolve the filename to an absolute one
// using PATH and friends.
char *ld_wrap_resolv(const char *filename){
  char *resolved = (char *)filename;
  if(!strchr(filename, '/')){
    char *path = ld_wrap_path();
    size_t pathlen = strlen(path);
    size_t filelen = strlen(filename);
    char current_path[pathlen+filelen+2];
    
    if(current_path != NULL){
      size_t j=0;
      for(size_t i=0; i<=pathlen; ++i){
        if(path[i] == 0 || path[i] == ':'){
          current_path[j] = '/';
          for(int k=0; k<filelen; ++k) current_path[j+1+k]=filename[k];
          current_path[j+filelen+1] = 0;
          if(ld_wrap_exe_p(current_path)){
            resolved = strdup(current_path);
            break;
          }else{
            j=0;
          }
        }else{
          current_path[j]=path[i];
          ++j;
        }
      }
    }
  }
  return resolved;
}

int execve(const char *filename, char *const argv[], char *const envp[]){
  const char *loader = getenv("LW_LOADER_PATH");
  char **argv_t = ld_wrap_argv(filename, argv);
#if(DEBUG)
  ld_wrap_log(loader, argv_t);
#endif
  int status = o_execve(loader, argv_t, envp);
  free(argv_t);
  return status;
}

int execv(const char *filename, char *const argv[]){
  char *const envp[] = {};
  return execve(filename, argv, envp);
}

int execvpe(const char *filename, char *const argv[], char *const envp[]){
  char *resolved = ld_wrap_resolv(filename);
  int status = execve(resolved, argv, envp);
  // execvp* have the "interesting" feature that they relaunch the command
  // using a shell if the first execve fails with ENOEXEC.
  if(status == ENOEXEC){
    int len;
    for(len=0; argv[len]; ++len);
    char *argv_t[len+2];
    argv_t[0] = (char *)_PATH_BSHELL;
    argv_t[1] = resolved;
    for(int i=1; argv[i]; ++i)
      argv_t[i+1] = argv[i];
    argv_t[len+1] = 0;
    status = execve((char *) _PATH_BSHELL, argv_t, envp);
  }
  free(resolved);
  return status;
}

int execvp(const char *filename, char *const argv[]){
  char *const envp[] = {};
  return execvpe(filename, argv, envp);
}

int execl(const char *filename, const char *arg, ...){
  size_t argc;
  va_list args;
  va_start(args, arg);
  for(argc=1; va_arg(args, const char*); ++argc);
  va_end(args);
  
  char *argv[argc + 1];
  va_start(args, arg);
  argv[0] = (char *)arg;
  for (size_t i=1; i<=argc; ++i)
    argv[i] = va_arg(args, char *);
  va_end(args);

  return execv(filename, argv);
}

int execlp(const char *filename, const char *arg, ...){
  size_t argc;
  va_list args;
  va_start(args, arg);
  for(argc=1; va_arg(args, const char*); ++argc);
  va_end(args);
  
  char *argv[argc + 1];
  va_start(args, arg);
  argv[0] = (char *)arg;
  for (size_t i=1; i<=argc; ++i)
    argv[i] = va_arg(args, char *);
  va_end(args);

  return execvp(filename, argv);
}

int execle(const char *filename, const char *arg, ...){
  size_t argc;
  va_list args;
  va_start(args, arg);
  for(argc=1; va_arg(args, const char*); ++argc);
  va_end(args);
  
  char *argv[argc + 1];
  va_start(args, arg);
  argv[0] = (char *)arg;
  for (size_t i=1; i<=argc; ++i)
    argv[i] = va_arg(args, char *);
  // I don't really think this is correct, but the original glibc source
  // seems to do the same kind of thing? Let's just hope nobody uses the
  // variadic variants of these anyway.
  const char *const *envp = va_arg(args, const char *const *);
  va_end(args);

  return execve(filename, argv, (char *const *)envp);
}
