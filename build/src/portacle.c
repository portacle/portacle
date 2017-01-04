#ifdef 	__MINGW64__
#define WIN
#elseif __MINGW32__
#define WIN
#elseif __APPLE__
#define MAC
#elseif __linux__
#define LIN
#endif

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

int pathcat(char *path, char *root, int c, ...){
  strcpy(path, root);
  
  va_list argp;
  va_start(argp, c);
  for(var i=0; i<c; ++i){
    char *part = va_arg(argp, char *);
    strcat(path, PATHSEP);
    strcat(path, part);
  }
  va_end(argp);
  return 0;
}

int launch_emacs(char *root){
  
}

int launch_git(char *root){
  char lw_loader[PATHLEN], lw_library[PATHLEN], ld_preload[PATHLEN], git[PATHLEN], xdg[PATHLEN];
  pathcat(lw_loader, root, 4, "usr", "lin", "lib", "ld-linux.so");
  pathcat(lw_library, root, 3, "usr", "lin", "lib");
  pathcat(ld_preload, root, 4, "usr", "lin", "lib", "ld-wrap.so");
  pathcat(git, root, 4, "git", "lin", "bin", "git");
  pathcat(xdg, root, 2, "config", "");
  set_env("LW_LOADER_PATH", lw_loader);
  set_env("LW_LIBRARY_PATH", lw_library);
  set_env("LD_PRELOAD", ld_preload);
  set_env("XDG_CONFIG_HOME", xdg);

  return launch(lw_loader_path, 3, "--library-path", lw_library, git);
}

int launch_sbcl(char *root){
  char sbcl_home[PATHLEN], sbcl_bin[PATHLEN], init[PATHLEN];
  pathcat(sbcl_home, root, 4, "sbcl", PLATFORM, "lib", "sbcl");
  pathcat(sbcl_bin, root, 4, "sbcl", PLATFORM, "bin", "sbcl");
  pathcat(init, root, 2, "config", "sbcl-init.lisp");
  set_env("SBCL_HOME", sbcl_home);
  set_env("LD_LIBRARY_PATH", usr);
  return launch(sbcl_bin, 3, "--no-sysinit", "--userinit", init);
}

int main(int argc, char **argv){
  char root[PATHLEN];
  if(!find_root(root)){
    return 1;
  }
  
  if(!set_env("ROOT", root)){
    return 2;
  }

  if(argc == 0){
    return launch_emacs(root);
  }else{
    // Dispatch
  }
  
  return 0;
}
