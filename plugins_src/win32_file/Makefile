
# wp8_file - Makefile for Windows NT
#
# It is assumed that the following environment variables have been set:
# Run vcvars32.bat before attempting to make and have erlang in your path
#

#
# Should one use the MT library as the emulator or the MD library
# like programs that load dll's actually should do? 
# Well, I hold for the later, so I'll try it as long as it works.
# I.E /MD as compiler option and link with msvcrt.lib.
#
ERL_TOP = C:\progra~1\erl5.1
LINK = link
MLFLAGS= /nologo /subsystem:console /machine:I386 /nodefaultlib 
MLIBS = kernel32.lib ws2_32.lib msvcrt.lib user32.lib comdlg32.lib

LIBDIR = ..\..\plugins\win32_file
OBJDIR = .

!if !exist($(LIBDIR)) 
!	if [mkdir $(LIBDIR)]
!	error "wp8_file: cannot create LIBDIR"
!	endif
!endif

!if !exist($(OBJDIR))
!	if [mkdir $(OBJDIR)]
!	error "wp8_file: cannot create OBJDIR"
!	endif
!endif

# Includes
#
INCLUDES = -I. -I$(ERL_TOP)\erl5.1\usr\include

# Libraries (static)
#

ERL_DLL_LIB = -I$(ERL_TOP)\usr\lib\erl_dll.lib

# Compiler options
# 
OPTS = /G5 /Ox /O2 /Ob2
CFLAGS = $(INCLUDES) /nologo /MD $(OPTS) -D__WIN32__ -DWIN32

# Object files
#
OBJS = $(OBJDIR)\wings_file_drv.obj	

DLLS = $(LIBDIR)\wings_file_drv.dll

BEAM = $(LIBDIR)\wp8_file.beam
ERLC = erlc
ERLC_FLAGS = -o $(LIBDIR)
#
# Targets
#

all:	$(DLLS) $(BEAM)

clean: 
	-del $(LIBDIR)\*.dll
	-del $(LIBDIR)\*.exp
	-del $(LIBDIR)\*.lib
	-del $(LIBDIR)\*.beam
	-del $(OBJDIR)\*.obj

$(LIBDIR)\wings_file_drv.dll:wings_file_drv.c
	$(CC) $(CFLAGS) /c /Fo$(OBJDIR)\wings_file_drv.obj wings_file_drv.c
	$(LINK) $(MLFLAGS) /dll /out:$(LIBDIR)\wings_file_drv.dll \
		$(OBJS) \
		$(ERL_DLL_LIB) \
		$(MLIBS)
	-del $(LIBDIR)\*.exp
	-del $(LIBDIR)\*.lib
	-del $(OBJDIR)\*.obj

$(LIBDIR)\wp8_file.beam: wp8_file.erl
	$(ERLC) $(ERLC_FLAGS) wp8_file.erl














