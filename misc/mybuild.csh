#!/bin/tcsh -f
# Time-stamp: <2015-12-10 11:43:01 kmodi>

# Generic script to build (without root access) any version of emacs from git.

# Example usages:
#   source mybuild.csh -- Install from latest origin/master.
#   source mybuild.csh -dirty -- Do not do make maintainer-clean and make bootstrap
#   source mybuild.csh -rev <COMMIT_HASH> -dirty -subdir test -- Dirty build for specified commit in test/
#   source mybuild.csh -noupdate -debug -- Only show the git info of last build

#   source mybuild.csh -rev emacs-24
#   source mybuild.csh -rev master
#   source mybuild.csh -debug -- Do not do the actual install, but update from git
#                                print useful echo statements and retain the internal
#                                variables.
#   source mybuild.csh -subdir test -- Install in test/ dir
#   source mybuild.csh -noupdate -- Install without updating from the git
#   source mybuild.csh -noinstall -- Build but do not install in ${MY_EMACS_INSTALL_DIR}
#   source mybuild.csh -noopt -subdir noimagemagick -- Build without optimization to a subdir "noimagemagick/"

# Update the following environment variables post-installation:
#   prepend PATH     with "${HOME}/usr_local/apps/${OSREV}/emacs/`basename ${MY_EMACS_INSTALL_DIR}`/bin"
#   prepend MANPATH  with "${HOME}/usr_local/apps/${OSREV}/emacs/`basename ${MY_EMACS_INSTALL_DIR}`/share/man"
#   prepend INFOPATH with "${HOME}/usr_local/apps/${OSREV}/emacs/`basename ${MY_EMACS_INSTALL_DIR}`/share/info"

# For RHEL 6, do "setenv MY_OSREV 6"
# "${HOME}/usr_local/${MY_OSREV}" is the directory that I use as
# --prefix for building libraries locally when the default library
# does not exist or is older than the requirements.

# I already have the env vars PATH, LD_LIBRARY_PATH, PKG_CONFIG_PATH
# and INCLUDE_PATH to include the respective directories in
# ${HOME}/usr_local/${MY_OSREV}.

# Ensure that PKG_CONFIG_PATH has /usr/share/pkgconfig for xproto.pc
# and related files. This is necessary for XFT and related X11
# features.. otherwise the fonts will look horrible.

# set emacs_rev       = "origin/master"
set emacs_rev       = "origin/emacs-25"
set emacs_gdb_build = 0
set no_git_update   = 0
set dirty_make      = 0
set install_sub_dir = ""
set no_install      = 0
set debug           = 0

while ($#argv)
    switch ($argv[1])
        case -rev:
            shift
            set emacs_rev = $argv[1]
            shift
            breaksw
        case -noopt: # no optimization for gdb debug
            set emacs_gdb_build = 1
            shift
            breaksw
        case -noupdate:
            set no_git_update = 1
            shift
            breaksw
        case -dirty:
            set dirty_make = 1
            shift
            breaksw
        case -subdir:
            shift
            set install_sub_dir = $argv[1]
            shift
            breaksw
        case -noinstall:
            set no_install = 1
            shift
            breaksw
        case -debug:
            set debug = 1
            shift
            breaksw
    endsw
end

if ( $debug ) then
    echo '======================================================================'
    echo '                      D E B U G      M O D E'
    echo '======================================================================'
endif

if ( ! ${no_git_update} ) then
    git fetch --all # fetch new branch names if any
    git checkout ${emacs_rev}
    git fetch --all
    git reset --hard ${emacs_rev}
    echo "Waiting for 5 seconds .. "
    sleep 5
endif

if ( "${install_sub_dir}" == "" ) then
    # If ${emacs_rev} is master, basename ${emacs_rev} -> master
    # If ${emacs_rev} is origin/emacs-24.5, basename ${emacs_rev} -> emacs-24.5
    set install_sub_dir = "`basename ${emacs_rev}`"
endif
setenv MY_EMACS_INSTALL_DIR "${HOME}/usr_local/apps/${MY_OSREV}/emacs/${install_sub_dir}"
if ( ! $debug ) then
    echo "Creating directory: ${MY_EMACS_INSTALL_DIR} .."
    mkdir -p ${MY_EMACS_INSTALL_DIR}
endif

# Basic configure command
setenv MY_EMACS_CONFIGURE "./configure --prefix=${MY_EMACS_INSTALL_DIR}"

# # Fri Oct 23 15:17:10 EDT 2015 - kmodi
# # http://debbugs.gnu.org/cgi/bugreport.cgi?bug=21738
# # (eww "http://www.braveclojure.com/basic-emacs")
# # Workaround to prevent emacs freeze when opening png files or browsing
# # websites containing png files on eww.
# setenv MY_EMACS_CONFIGURE "${MY_EMACS_CONFIGURE} --without-imagemagick"
#
# Tue Oct 27 18:45:32 EDT 2015 - kmodi
# Now above is not required after building Imagemagick with --without-threads
# configure option.

# Initialize the configure flag variables
set emacs_configure_CFLAGS   = ''
set emacs_configure_CXXFLAGS = ''
set emacs_configure_CPPFLAGS = ''
set emacs_configure_LDFLAGS  = ''

set emacs_configure_CPPFLAGS = 'CPPFLAGS="'"-fgnu89-inline -I${HOME}/usr_local/${MY_OSREV}/include -I/usr/include/freetype2 -I/usr/include"
# -L${HOME}/usr_local/${MY_OSREV}/lib required for libgpm (GPM feature)
# -L${HOME}/usr_local/${MY_OSREV}/lib64 required for libgif (GIF feature)
set emacs_configure_LDFLAGS  = 'LDFLAGS="'"-L${HOME}/usr_local/${MY_OSREV}/lib -L${HOME}/usr_local/${MY_OSREV}/lib64"

# For Debug
if ( ${emacs_gdb_build} ) then
    set emacs_configure_CFLAGS   = 'CFLAGS="'"-ggdb3 -O0"
    set emacs_configure_CXXFLAGS = 'CXXFLAGS="'"-ggdb3 -O0"
    set emacs_configure_LDFLAGS  = "${emacs_configure_LDFLAGS} -ggdb3"
endif

# Close the double quotes
if ( ! ( "${emacs_configure_CFLAGS}" == "" ) ) then
    set emacs_configure_CFLAGS = "${emacs_configure_CFLAGS}"'"'
endif
if ( ! ( "${emacs_configure_CXXFLAGS}" == "" ) ) then
    set emacs_configure_CXXFLAGS = "${emacs_configure_CXXFLAGS}"'"'
endif
if ( ! ( "${emacs_configure_CPPFLAGS}" == "" ) ) then
    set emacs_configure_CPPFLAGS = "${emacs_configure_CPPFLAGS}"'"'
endif
if ( ! ( "${emacs_configure_LDFLAGS}" == "" ) ) then
    set emacs_configure_LDFLAGS = "${emacs_configure_LDFLAGS}"'"'
endif

setenv MY_EMACS_CONFIGURE "${MY_EMACS_CONFIGURE} ${emacs_configure_CPPFLAGS} ${emacs_configure_CFLAGS} ${emacs_configure_CXXFLAGS} ${emacs_configure_LDFLAGS}"

echo ""
echo "  MY_EMACS_INSTALL_DIR = ${MY_EMACS_INSTALL_DIR}"
echo ""
echo "  MY_EMACS_CONFIGURE = ${MY_EMACS_CONFIGURE}"
echo ""

if ( ! $debug ) then
    echo "Waiting for 5 seconds .. Press Ctrl+C to cancel this installation."
    sleep 5

    if ( ! ${dirty_make} ) then
        make maintainer-clean
    endif

    sed -i 's|/usr/local|${MY_EMACS_INSTALL_DIR}|g' src/epaths.in
    sed -i 's|./configure|${MY_EMACS_CONFIGURE}|g'  GNUmakefile

    if ( ${dirty_make} ) then
        make
    else
        # Do NOT call autoreconf. "make bootstrap" below will call autoreconf
        # with proper arguments.
        # The `make bootstrap' step is required for a clean fresh install
        make bootstrap
    endif
    if ( ! ${no_install} ) then
        make install
    endif
endif

# Save the current commit information to the install dir
set current_commit_hash = `git rev-parse HEAD`
if ( ! $debug ) then
    set build_info_file     = "${MY_EMACS_INSTALL_DIR}/build_info"
else
    set build_info_file     = "/tmp/${USER}_emacs_build_info_debug"
endif

if ( -e ${build_info_file} ) then
    set prev_build_time_stamp = `\grep -oiE '[0-9]{4}/[0-9]{2}/[0-9]{2}.*?[A-Z][a-z]{2}' ${build_info_file} | tr " /:" "_"`
    \mv ${build_info_file} ${build_info_file}.${prev_build_time_stamp}
endif
cat /dev/null >! ${build_info_file}
if ( ! $debug ) then
    echo "Build date        : `date +'%Y/%m/%d %H:%M %a'`" >> ${build_info_file}
endif
echo "Savannah Git link : http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=${current_commit_hash}" >> ${build_info_file}
echo "" >> ${build_info_file}
git log -n 1 --pretty=full ${current_commit_hash} >> ${build_info_file}

if ( $debug ) then
    cat ${build_info_file}
endif

if ( ! $debug ) then
    # I use the Exuberant ctags (now Universal ctags build from its github
    # repo). So rename the binary and man of "ctags" that ship with emacs so
    # that I do not end up using the incorrect ctags.
    if ( -e ${MY_EMACS_INSTALL_DIR}/bin/ctags ) then
        \mv ${MY_EMACS_INSTALL_DIR}/bin/ctags ${MY_EMACS_INSTALL_DIR}/bin/ctags_emacs
    endif
    if ( -e ${MY_EMACS_INSTALL_DIR}/share/man/man1/ctags.1.gz ) then
        \mv ${MY_EMACS_INSTALL_DIR}/share/man/man1/ctags.1.gz ${MY_EMACS_INSTALL_DIR}/share/man/man1/ctags_emacs.1.gz
    endif

    # unset all internal variables
    unset {emacs_configure_CFLAGS}
    unset {emacs_configure_CPPFLAGS}
    unset {emacs_configure_CXXFLAGS}
    unset {emacs_configure_LDFLAGS}
    unset {prev_build_time_stamp}
    unset {build_info_file}
    unset {current_commit_hash}
    unset {emacs_rev}
    unset {emacs_gdb_build}
    unset {no_git_update}
    unset {dirty_make}
    unset {install_sub_dir}
    unset {no_install}
    unset {debug}
endif

# Helpful tcsh alias associated with ${build_info_file}
# # Get info about the emacs built from git and also copy the web address to this
# # commit on emacs savannah git
# alias ?e 'cat `which emacs | rev | cut -d/ -f2- | rev`/../build_info | tee /tmp/${USER}_emacs_build_info; \\
#           \grep -oE '"'"'http:.*'"'"' /tmp/${USER}_emacs_build_info | xi; \\
#          '
