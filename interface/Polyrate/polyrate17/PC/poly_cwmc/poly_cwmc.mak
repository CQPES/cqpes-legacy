# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=poly_cwmc - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to poly_cwmc - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "poly_cwmc - Win32 Release" && "$(CFG)" !=\
 "poly_cwmc - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "poly_cwmc.mak" CFG="poly_cwmc - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "poly_cwmc - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "poly_cwmc - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "poly_cwmc - Win32 Debug"
RSC=rc.exe
F90=fl32.exe

!IF  "$(CFG)" == "poly_cwmc - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\poly_cwmc.exe"

CLEAN : 
	-@erase ".\Release\poly_cwmc.exe"
	-@erase ".\Release\fromblas.obj"
	-@erase ".\Release\poly40.obj"
	-@erase ".\Release\polysz.obj"
	-@erase ".\Release\poly31.obj"
	-@erase ".\Release\polyrr.obj"
	-@erase ".\Release\dattim.obj"
	-@erase ".\Release\intbsv3.obj"
	-@erase ".\Release\polyhl.obj"
	-@erase ".\Release\hooks.obj"
	-@erase ".\Release\polyag.obj"
	-@erase ".\Release\rtpjac.obj"
	-@erase ".\Release\energetics.obj"
	-@erase ".\Release\givtst.obj"
	-@erase ".\Release\intbsv2.obj"
	-@erase ".\Release\headr.obj"
	-@erase ".\Release\ef.obj"
	-@erase ".\Release\ivtstm.obj"
	-@erase ".\Release\acespoly.obj"
	-@erase ".\Release\main.obj"
	-@erase ".\Release\intbsv1.obj"
	-@erase ".\Release\interface.obj"
	-@erase ".\Release\polymq.obj"
	-@erase ".\Release\cwmc.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Ox /I "Release/" /c /nologo
# ADD F90 /Ox /I "Release/" /c /nologo
F90_PROJ=/Ox /I "Release/" /c /nologo /Fo"Release/" 
F90_OBJS=.\Release/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/poly_cwmc.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/poly_cwmc.pdb" /machine:I386 /out:"$(OUTDIR)/poly_cwmc.exe" 
LINK32_OBJS= \
	"$(INTDIR)/fromblas.obj" \
	"$(INTDIR)/poly40.obj" \
	"$(INTDIR)/polysz.obj" \
	"$(INTDIR)/poly31.obj" \
	"$(INTDIR)/polyrr.obj" \
	"$(INTDIR)/dattim.obj" \
	"$(INTDIR)/intbsv3.obj" \
	"$(INTDIR)/polyhl.obj" \
	"$(INTDIR)/hooks.obj" \
	"$(INTDIR)/polyag.obj" \
	"$(INTDIR)/rtpjac.obj" \
	"$(INTDIR)/energetics.obj" \
	"$(INTDIR)/givtst.obj" \
	"$(INTDIR)/intbsv2.obj" \
	"$(INTDIR)/headr.obj" \
	"$(INTDIR)/ef.obj" \
	"$(INTDIR)/ivtstm.obj" \
	"$(INTDIR)/acespoly.obj" \
	"$(INTDIR)/main.obj" \
	"$(INTDIR)/intbsv1.obj" \
	"$(INTDIR)/interface.obj" \
	"$(INTDIR)/polymq.obj" \
	"$(INTDIR)/cwmc.obj"

"$(OUTDIR)\poly_cwmc.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "poly_cwmc - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\poly_cwmc.exe"

CLEAN : 
	-@erase ".\Debug\poly_cwmc.exe"
	-@erase ".\Debug\fromblas.obj"
	-@erase ".\Debug\intbsv1.obj"
	-@erase ".\Debug\hooks.obj"
	-@erase ".\Debug\energetics.obj"
	-@erase ".\Debug\ivtstm.obj"
	-@erase ".\Debug\polysz.obj"
	-@erase ".\Debug\polyrr.obj"
	-@erase ".\Debug\dattim.obj"
	-@erase ".\Debug\polyhl.obj"
	-@erase ".\Debug\cwmc.obj"
	-@erase ".\Debug\givtst.obj"
	-@erase ".\Debug\poly40.obj"
	-@erase ".\Debug\intbsv3.obj"
	-@erase ".\Debug\acespoly.obj"
	-@erase ".\Debug\poly31.obj"
	-@erase ".\Debug\interface.obj"
	-@erase ".\Debug\main.obj"
	-@erase ".\Debug\ef.obj"
	-@erase ".\Debug\intbsv2.obj"
	-@erase ".\Debug\headr.obj"
	-@erase ".\Debug\polymq.obj"
	-@erase ".\Debug\polyag.obj"
	-@erase ".\Debug\rtpjac.obj"
	-@erase ".\Debug\poly_cwmc.ilk"
	-@erase ".\Debug\poly_cwmc.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Zi /I "Debug/" /c /nologo
# ADD F90 /Ox /Zi /I "Debug/" /c /nologo
F90_PROJ=/Ox /Zi /I "Debug/" /c /nologo /Fo"Debug/" /Fd"Debug/poly_cwmc.pdb" 
F90_OBJS=.\Debug/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/poly_cwmc.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/poly_cwmc.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/poly_cwmc.exe" 
LINK32_OBJS= \
	"$(INTDIR)/fromblas.obj" \
	"$(INTDIR)/intbsv1.obj" \
	"$(INTDIR)/hooks.obj" \
	"$(INTDIR)/energetics.obj" \
	"$(INTDIR)/ivtstm.obj" \
	"$(INTDIR)/polysz.obj" \
	"$(INTDIR)/polyrr.obj" \
	"$(INTDIR)/dattim.obj" \
	"$(INTDIR)/polyhl.obj" \
	"$(INTDIR)/cwmc.obj" \
	"$(INTDIR)/givtst.obj" \
	"$(INTDIR)/poly40.obj" \
	"$(INTDIR)/intbsv3.obj" \
	"$(INTDIR)/acespoly.obj" \
	"$(INTDIR)/poly31.obj" \
	"$(INTDIR)/interface.obj" \
	"$(INTDIR)/main.obj" \
	"$(INTDIR)/ef.obj" \
	"$(INTDIR)/intbsv2.obj" \
	"$(INTDIR)/headr.obj" \
	"$(INTDIR)/polymq.obj" \
	"$(INTDIR)/polyag.obj" \
	"$(INTDIR)/rtpjac.obj"

"$(OUTDIR)\poly_cwmc.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

################################################################################
# Begin Target

# Name "poly_cwmc - Win32 Release"
# Name "poly_cwmc - Win32 Debug"

!IF  "$(CFG)" == "poly_cwmc - Win32 Release"

!ELSEIF  "$(CFG)" == "poly_cwmc - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\util\dattim.f

"$(INTDIR)\dattim.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\rtpjac.f
DEP_F90_RTPJA=\
	".\..\..\src\param.inc"\
	".\..\..\src\percon.inc"\
	".\..\..\src\common.inc"\
	

"$(INTDIR)\rtpjac.obj" : $(SOURCE) $(DEP_F90_RTPJA) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\polysz.f
DEP_F90_POLYS=\
	".\..\..\src\param.inc"\
	".\..\..\src\percon.inc"\
	".\..\..\src\common.inc"\
	

"$(INTDIR)\polysz.obj" : $(SOURCE) $(DEP_F90_POLYS) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\polyrr.f
DEP_F90_POLYR=\
	".\..\..\src\param.inc"\
	".\..\..\src\percon.inc"\
	".\..\..\src\common.inc"\
	

"$(INTDIR)\polyrr.obj" : $(SOURCE) $(DEP_F90_POLYR) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\polymq.f
DEP_F90_POLYM=\
	".\..\..\src\param.inc"\
	".\..\..\src\percon.inc"\
	".\..\..\src\common.inc"\
	

"$(INTDIR)\polymq.obj" : $(SOURCE) $(DEP_F90_POLYM) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\polyhl.f
DEP_F90_POLYH=\
	".\..\..\src\param.inc"\
	".\..\..\src\percon.inc"\
	".\..\..\src\common.inc"\
	

"$(INTDIR)\polyhl.obj" : $(SOURCE) $(DEP_F90_POLYH) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\polyag.f
DEP_F90_POLYA=\
	".\..\..\src\param.inc"\
	".\..\..\src\percon.inc"\
	".\..\..\src\common.inc"\
	

"$(INTDIR)\polyag.obj" : $(SOURCE) $(DEP_F90_POLYA) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\poly40.f
DEP_F90_POLY4=\
	".\..\..\src\param.inc"\
	".\..\..\src\percon.inc"\
	".\..\..\src\common.inc"\
	

"$(INTDIR)\poly40.obj" : $(SOURCE) $(DEP_F90_POLY4) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\poly31.f
DEP_F90_POLY3=\
	".\..\..\src\param.inc"\
	".\..\..\src\percon.inc"\
	".\..\..\src\common.inc"\
	

"$(INTDIR)\poly31.obj" : $(SOURCE) $(DEP_F90_POLY3) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\main.f
DEP_F90_MAIN_=\
	".\..\..\src\param.inc"\
	".\..\..\src\percon.inc"\
	".\..\..\src\common.inc"\
	

"$(INTDIR)\main.obj" : $(SOURCE) $(DEP_F90_MAIN_) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\ivtstm.f
DEP_F90_IVTST=\
	".\..\..\src\param.inc"\
	".\..\..\src\percon.inc"\
	".\..\..\src\common.inc"\
	

"$(INTDIR)\ivtstm.obj" : $(SOURCE) $(DEP_F90_IVTST) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\interface.f
DEP_F90_INTER=\
	".\..\..\src\param.inc"\
	".\..\..\src\percon.inc"\
	".\..\..\src\common.inc"\
	

"$(INTDIR)\interface.obj" : $(SOURCE) $(DEP_F90_INTER) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\intbsv3.f
DEP_F90_INTBS=\
	".\..\..\src\param.inc"\
	".\..\..\src\percon.inc"\
	".\..\..\src\common.inc"\
	

"$(INTDIR)\intbsv3.obj" : $(SOURCE) $(DEP_F90_INTBS) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\intbsv2.f
DEP_F90_INTBSV=\
	".\..\..\src\param.inc"\
	".\..\..\src\percon.inc"\
	".\..\..\src\common.inc"\
	

"$(INTDIR)\intbsv2.obj" : $(SOURCE) $(DEP_F90_INTBSV) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\intbsv1.f
DEP_F90_INTBSV1=\
	".\..\..\src\param.inc"\
	".\..\..\src\percon.inc"\
	".\..\..\src\common.inc"\
	

"$(INTDIR)\intbsv1.obj" : $(SOURCE) $(DEP_F90_INTBSV1) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\hooks.f
DEP_F90_HOOKS=\
	".\..\..\src\param.inc"\
	".\..\..\src\percon.inc"\
	".\..\..\src\common.inc"\
	

"$(INTDIR)\hooks.obj" : $(SOURCE) $(DEP_F90_HOOKS) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\headr.f
DEP_F90_HEADR=\
	".\..\..\src\param.inc"\
	

"$(INTDIR)\headr.obj" : $(SOURCE) $(DEP_F90_HEADR) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\givtst.f
DEP_F90_GIVTS=\
	".\..\..\src\param.inc"\
	".\..\..\src\percon.inc"\
	

"$(INTDIR)\givtst.obj" : $(SOURCE) $(DEP_F90_GIVTS) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\fromblas.f
DEP_F90_FROMB=\
	".\..\..\src\param.inc"\
	".\..\..\src\percon.inc"\
	

"$(INTDIR)\fromblas.obj" : $(SOURCE) $(DEP_F90_FROMB) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\energetics.f
DEP_F90_ENERG=\
	".\..\..\src\param.inc"\
	".\..\..\src\percon.inc"\
	".\..\..\src\common.inc"\
	

"$(INTDIR)\energetics.obj" : $(SOURCE) $(DEP_F90_ENERG) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\ef.f
DEP_F90_EF_F26=\
	".\..\..\src\param.inc"\
	".\..\..\src\percon.inc"\
	

"$(INTDIR)\ef.obj" : $(SOURCE) $(DEP_F90_EF_F26) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\src\acesrate\acespoly.f

"$(INTDIR)\acespoly.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\polyrate9.3\poten\cwmc.f

"$(INTDIR)\cwmc.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
# End Target
# End Project
################################################################################
