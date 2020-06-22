#!/usr/bin/env bash
# Time-stamp: <2020-06-22 10:25:50 kmodi>

# Generic script to build (without root access) any version of emacs from git.

# Example usages:
#   ./mybuild.sh -- Install from ${emacs_rev}
#   ./mybuild.sh --quick -- Do not do 'make bootstrap' from ${emacs_rev}
#   ./mybuild.sh --noopt -- Build without optimization (to allow gdb debug) from ${emacs_rev}
#   ./mybuild.sh --noopt --rev origin/master -- Build without optimization (to allow gdb debug) using the master branch

#   ./mybuild.sh --rev <COMMIT_HASH> --quick --subdir test -- Quick build for specified commit to "test/"
#   ./mybuild.sh --noupdate --debug -- Only show the git info of last build
#   ./mybuild.sh --rev origin/emacs-24
#   ./mybuild.sh --debug -- Do not do the actual install, but update from git
#                                print useful echo statements and retain the internal
#                                variables.
#   ./mybuild.sh --subdir test -- Install in test/ dir
#   ./mybuild.sh --noupdate -- Install without updating from the git
#   ./mybuild.sh --noinstall -- Build but do not install in ${MY_EMACS_INSTALL_DIR}
#   ./mybuild.sh --noopt --subdir noimagemagick -- Build without optimization to a subdir "noimagemagick/"
#   ./mybuild.sh --noupdate --quick --noinfo --noelc --noinstall -- Quick builds without git clone, no info or elc building and no install; useful during git bisects

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

set -euo pipefail # http://redsymbol.net/articles/unofficial-bash-strict-mode
IFS=$'\n\t'

# emacs_rev="origin/master"
emacs_rev="origin/emacs-25"
emacs_debug_build=0
quick_make=0
install_sub_dir=""
no_git_update=0
no_install=0
no_info=0
no_elc=0
debug=0

dquote='"'

while [ $# -gt 0 ]
do
    case "$1" in
        "-r"|"--rev" ) shift
                       emacs_rev="$1";;
        "-O"|"--noopt" ) emacs_debug_build=1;; # no optimization for gdb debug build
        "-q"|"--quick" ) quick_make=1;;
        "-s"|"--subdir" ) shift
                          install_sub_dir="$1";;
        "-U"|"--noupdate" ) no_git_update=1;;
        "-I"|"--noinstall" ) no_install=1;;
        "-N"|"--noinfo" ) no_info=1;;
        "-E"|"--noelc" ) no_elc=1;;
        "-d"|"--debug" ) debug=1;;
        * ) echo >&2 "Error: Invalid option: $*"
    esac
    shift # expose next argument
done

if [[ $debug -eq 1 ]]
then
    echo '======================================================================'
    echo '                      D E B U G      M O D E'
    echo '======================================================================'
fi

# If ${emacs_rev} is master, ${emacs_rev_basename} = master
# If ${emacs_rev} is origin/emacs-24.5, ${emacs_rev_basename} = emacs-24.5
emacs_rev_basename=$(basename "${emacs_rev}")

if [[ ${no_git_update} -eq 0 ]]
then
    git fetch --all # fetch new branch names if any
    git checkout "${emacs_rev_basename}"
    git pull
    echo "Waiting for 5 seconds .. "
    sleep 5
fi

if [[ -z "${install_sub_dir}" ]]
then
    install_sub_dir="${emacs_rev_basename}"
fi

export MY_EMACS_INSTALL_DIR="${HOME}/usr_local/apps/${MY_OSREV}/emacs/${install_sub_dir}"

if [[ $debug -eq 0 ]]
then
    echo "Creating directory: ${MY_EMACS_INSTALL_DIR} .."
    mkdir -p "${MY_EMACS_INSTALL_DIR}"
fi

# Basic configure command
# Use --program-transform-name to name the emacs-packaged ctags as ctags_emacs
# because I like using the ctags binary instead that's built from Universal Ctags.
# https://www.topbug.net/blog/2016/11/10/installing-emacs-from-source-avoid-the-conflict-of-ctags/

# Tue May 14 13:16:25 EDT 2019 - kmodi
# Now emacs doesn't compile with imagemagick by default:
# https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=d2dea70415ca7ec390a2de11b224ab4cbb2c6b55

# export MY_EMACS_CONFIGURE="./configure --with-modules --prefix=${MY_EMACS_INSTALL_DIR} --program-transform-name='s/^ctags$/ctags_emacs/' --with-harfbuzz --with-dumping=unexec"
# export MY_EMACS_CONFIGURE="./configure --with-modules --prefix=${MY_EMACS_INSTALL_DIR} --program-transform-name='s/^ctags$/ctags_emacs/' --with-harfbuzz"
# export MY_EMACS_CONFIGURE="./configure --with-modules --with-imagemagick --prefix=${MY_EMACS_INSTALL_DIR} --program-transform-name='s/^ctags$/ctags_emacs/' --with-harfbuzz --without-cairo"
export MY_EMACS_CONFIGURE="./configure --with-modules --with-imagemagick --prefix=${MY_EMACS_INSTALL_DIR} --program-transform-name='s/^ctags$/ctags_emacs/' --with-harfbuzz"

# # Fri Oct 23 15:17:10 EDT 2015 - kmodi
# # http://debbugs.gnu.org/cgi/bugreport.cgi?bug=21738
# # (eww "http://www.braveclojure.com/basic-emacs")
# # Workaround to prevent emacs freeze when opening png files or browsing
# # websites containing png files on eww.
# export MY_EMACS_CONFIGURE="${MY_EMACS_CONFIGURE} --without-imagemagick"
#
# Tue Oct 27 18:45:32 EDT 2015 - kmodi
# Now above is not required after building Imagemagick with --without-threads
# configure option.

# Initialize the configure flag variables
emacs_configure_CFLAGS=""
emacs_configure_CXXFLAGS=""
emacs_configure_CPPFLAGS=""
emacs_configure_LDFLAGS=""

CAD_INC="/cad/adi/apps/gnu/linux/x86_64/${MY_OSREV}/include/" # Contains gif_lib.h for libgif 5.1.0
CAD_LIB="/cad/adi/apps/gnu/linux/x86_64/${MY_OSREV}/lib/"     # Contains libgif .so files

# -I${STOW_PKGS_TARGET}/include required for lgmp
emacs_configure_CPPFLAGS="CPPFLAGS=${dquote}-I${STOW_PKGS_TARGET}/include -I${CAD_INC} -I${HOME}/usr_local/${MY_OSREV}/include -I/usr/include/freetype2 -I/usr/include"
# -L${HOME}/usr_local/${MY_OSREV}/lib required for libgpm (GPM feature)
# -L${HOME}/usr_local/${MY_OSREV}/lib64 required for libgif (GIF feature)
emacs_configure_LDFLAGS="LDFLAGS=${dquote}-L${STOW_PKGS_TARGET}/lib -L${STOW_PKGS_TARGET}/lib64 -L${CAD_LIB} -L${HOME}/usr_local/${MY_OSREV}/lib -L${HOME}/usr_local/${MY_OSREV}/lib64"

if [[ ${emacs_debug_build} -eq 1 ]] # For Debug
then
    # http://git.savannah.gnu.org/cgit/emacs.git/plain/etc/DEBUG
    export MY_EMACS_CONFIGURE="${MY_EMACS_CONFIGURE} --enable-checking='yes,glyphs' --enable-check-lisp-object-type"
    # If using --enable-check-lisp-object-type causes issues in the
    # representation of error_symbol in gdb, converting it from a
    # scalar to a vector, comment it out (
    # http://debbugs.gnu.org/cgi/bugreport.cgi?bug=23424#54 ).
    # About -g vs -ggdb options: http://gcc.gnu.org/onlinedocs/gcc/Debugging-Options.html
    emacs_configure_CFLAGS="CFLAGS=${dquote}-ggdb3 -Og"
    emacs_configure_CXXFLAGS="CXXFLAGS=${dquote}-ggdb3 -Og"
    emacs_configure_LDFLAGS="${emacs_configure_LDFLAGS} -ggdb3"
else
    # http://emacs.stackexchange.com/a/19839/115
    emacs_configure_CFLAGS="CFLAGS=${dquote}-O2 -march=native"
fi

# Close the double quotes
if [[ ! -z "${emacs_configure_CFLAGS}" ]]
then
    emacs_configure_CFLAGS="${emacs_configure_CFLAGS}${dquote}"
fi
if [[ ! -z "${emacs_configure_CXXFLAGS}" ]]
then
    emacs_configure_CXXFLAGS="${emacs_configure_CXXFLAGS}${dquote}"
fi
if [[ ! -z "${emacs_configure_CPPFLAGS}" ]]
then
    emacs_configure_CPPFLAGS="${emacs_configure_CPPFLAGS}${dquote}"
fi
if [[ ! -z "${emacs_configure_LDFLAGS}" ]]
then
    emacs_configure_LDFLAGS="${emacs_configure_LDFLAGS}${dquote}"
fi

export MY_EMACS_CONFIGURE="${MY_EMACS_CONFIGURE} ${emacs_configure_CPPFLAGS} ${emacs_configure_CFLAGS} ${emacs_configure_CXXFLAGS} ${emacs_configure_LDFLAGS}"

echo ""
echo "  MY_EMACS_INSTALL_DIR = ${MY_EMACS_INSTALL_DIR}"
echo ""
echo "  MY_EMACS_CONFIGURE = ${MY_EMACS_CONFIGURE}"
echo ""

if [[ $debug -eq 0 ]]
then
    echo "Waiting for 5 seconds .. Press Ctrl+C to cancel this installation."
    sleep 5

    if [[ ! -f "./configure" ]]
    then
        ./autogen.sh all
    fi

    # Call ./configure with the required CPPFLAGS, CFLAGS, etc so that
    # Makefile is generated successfully. Without the below sed command,
    # build fails with the below error:
    #   checking for GifMakeMapObject in -lgif... no
    #   checking for EGifPutExtensionLast in -lgif... no
    #   checking for EGifPutExtensionLast in -lungif... no
    #   configure: error: The following required libraries were not found:
    #        libgif/libungif
    #   Maybe some development libraries/packages are missing?
    #   If you don't want to link with them give
    #        --with-gif=no
    #   as options to configure
    #   make: *** [Makefile] Error 1
    sed -i_orig 's|./configure|'"${MY_EMACS_CONFIGURE}"'|g' GNUmakefile

    # Do not build info files
    if [[ ${no_info} -eq 1 ]]
    then
        sed -r -i.orig 's/^(\s*all:.*)info/\1/' Makefile.in
    fi

    # Do not build .elc files
    if [[ ${no_elc} -eq 1 ]]
    then
        # In order to skip .elc file generation,
        # replace below:
        #   @echo Directories for loaddefs: ${SUBDIRS_ALMOST}
        #   $(AM_V_GEN)$(emacs) -l autoload \
            #       --eval '(setq autoload-ensure-writable t)' \
            #       --eval '(setq autoload-builtin-package-versions t)' \
            #       --eval '(setq generated-autoload-file (expand-file-name (unmsys--file-name "$@")))' \
            #       -f batch-update-autoloads ${SUBDIRS_ALMOST}
        # with:
        #   true
        sed -r -i.orig 's/^(\s*)@echo/\1true\n\0/'lisp/Makefile.in
        sed -r -i '/^\s*@echo/,/-f batch/d' lisp/Makefile.in
    fi

    # The below step is needed as we would need to rebuild all the Makefiles
    # when switching branches (e.g. from emacs-25 to master, or vice-versa),
    # or when git bisecting.
    eval "${MY_EMACS_CONFIGURE}"

    if [[ ${quick_make} -eq 1 ]]
    then
        make
    else
        # Do NOT call autoreconf. "make bootstrap" below will call autoreconf
        # with proper arguments.
        # The `make bootstrap' step is required for a clean fresh install.
        make bootstrap
    fi

    if [[ ${no_install} -eq 0 ]]
    then
        make install
    fi
fi

# Save the current commit information to the install dir
current_commit_hash=$(git rev-parse HEAD)
if [[ $debug -eq 0 ]]
then
    build_info_file="${MY_EMACS_INSTALL_DIR}/build_info"
else
    build_info_file="/tmp/${USER}_emacs_build_info_debug"
fi

if [[ -e ${build_info_file} ]]
then
    prev_build_time_stamp=$(grep -oiE '[0-9]{4}/[0-9]{2}/[0-9]{2}.*?[A-Z][a-z]{2}' "${build_info_file}" | tr " /:" "_")
    mv "${build_info_file}" "${build_info_file}.${prev_build_time_stamp}"
fi
cat /dev/null > "${build_info_file}"
if [[ $debug -eq 0 ]]
then
    echo "Build date        : $(date +'%Y/%m/%d %H:%M %a')" >> "${build_info_file}"
fi
echo -e "Savannah Git link : http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=${current_commit_hash}\n" >> "${build_info_file}"
git log -n 1 --pretty=full "${current_commit_hash}" >> "${build_info_file}"

if [[ $debug -eq 1 ]]
then
    cat "${build_info_file}"
fi

# Restore the original files
if [[ -f GNUmakefile_orig ]]
then
    mv GNUmakefile{_orig,}
fi
if [[ -f Makefile.in.orig ]]
then
    mv Makefile.in{.orig,}
fi
if [[ -f lisp/Makefile.in.orig ]]
then
    mv lisp/Makefile.in{.orig,}
fi

# Helpful tcsh alias associated with ${build_info_file}
# # Get info about the emacs built from git and also copy the web address to this
# # commit on emacs savannah git
# alias ?e 'cat `which emacs | rev | cut -d/ -f2- | rev`/../build_info | tee /tmp/${USER}_emacs_build_info; \\
#           \grep -oE '"'"'http:.*'"'"' /tmp/${USER}_emacs_build_info | xi; \\
#          '
