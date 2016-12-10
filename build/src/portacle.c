#include <windows.h>
#include <stdlib.h>
#include <string.h>
#include <shellapi.h>
#include <shlwapi.h>

int main(){
  TCHAR root[MAX_PATH];
  if(!GetModuleFileName(NULL, root, MAX_PATH)){
    return 1;
  }

  PathRemoveFileSpec(root);
  if(!SetEnvironmentVariable("ROOT", root)){
    return 2;
  }

  TCHAR emacs_batch[MAX_PATH];
  strcpy(emacs_batch, root);
  strcat(emacs_batch, "\\emacs\\win\\emacs.bat");

  PROCESS_INFORMATION process_info = {0};
  STARTUPINFO startup_info = {0};
  if(!CreateProcess(NULL,
                    emacs_batch,
                    NULL,
                    NULL,
                    FALSE,
                    CREATE_NO_WINDOW,
                    NULL, 
                    NULL, 
                    &startup_info,
                    &process_info)){
    return 3;
  }
  return 0;
}
