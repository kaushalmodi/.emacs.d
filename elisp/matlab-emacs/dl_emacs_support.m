function dl_emacs_support(varargin)
% Download MATLAB support files for Emacs
%
% DL_EMACS_SUPPRT - download all Emacs support files into the
%   current directory.
%
% DL_EMACS_SUPPORT(FILESET) - download a FILESET of Emacs support.
% Sets are:
%  core - Just the core MATLAB support files.
%  tlc - Just the core MATLAB/TLC support files.
%  cedet - Core, plus additional support for MATLAB using CEDET support.
%          Learn more about CEDET at: http://cedet.sf.net
%  support - Just the build files and READMEs for compiling.
%  all - All files
%
% DL_EMACS_SUPPORT(FILESET,DEST) - download FILESET and save in
%   destination directory DEST 

    po = inputParser;
    
    addOptional(po, 'fileset', 'all', @ischar)
    addOptional(po, 'destination', pwd, @ischar)
    
    po.parse(varargin{:});

    stuff = po.Results;
    
    if exist(stuff.destination,'dir') ~= 7
        error(['The folder: ''',stuff.destination, ''', does not exist.']);
    end

    coreFiles = { 'matlab-load.el' 'matlab.el' 'mlint.el' ...
                  'matlab-publish.el' 'company-matlab-shell.el' ...
                  'toolbox/emacsinit.m' 'toolbox/opentoline.m' };
    tlcFiles = { 'tlc.el' };
    cedetFiles = { 'cedet-matlab.el' 'semantic-matlab.el' ...
                   'semanticdb-matlab.el' 'templates/srecode-matlab.srt' };
    supportFiles = { 'dl_emacs_support.m' 'README' 'Makefile' ...
                     'Project.ede' 'INSTALL' 'ChangeLog' ...
                     'templates/Project.ede' 'templates/Makefile'};
    
    switch stuff.fileset
      case 'core'
        mktoolboxdir
        getfiles(coreFiles);
      case 'tlc'
        mktoolboxdir
        getfiles(coreFiles);
        getfiles(tlcFiles);
      case 'cedet'
        mktoolboxdir
        getfiles(coreFiles);
        mktemplatedir;
        getfiles(cedetFiles);
      case 'support'
        mktemplatedir;
        getfiles(supportFiles);
      case 'all'
        mktoolboxdir
        getfiles(coreFiles);
        getfiles(tlcFiles);
        mktemplatedir;
        getfiles(cedetFiles);
        getfiles(supportFiles);
      otherwise
        error('Unknown fileset %s.', stuff.fileset);
    end    

    function mktemplatedir
        templateDir = fullfile(stuff.destination,'templates');
        if ~exist(templateDir,'dir')
            mkdir(templateDir);
        end
    end
    
    function mktoolboxdir
        toolboxDir = fullfile(stuff.destination,'toolbox');
        if ~exist(toolboxDir,'dir')
            mkdir(toolboxDir);
        end
    end

    function getfiles(fList)
        for i = 1:length(fList)
            file = fList{i};
            destFullFile = fullfile(stuff.destination,file);
            [ contents status ] = ...
                urlread(['http://matlab-emacs.cvs.sourceforge.net/viewvc/*checkout*/matlab-emacs/matlab-emacs/',...
                         file,'?revision=HEAD']);
            if ~status
                fprintf('Unable to download %s.\n', file);
            else
                fid = fopen(destFullFile,'w');
                fwrite(fid,contents);
                fclose(fid);
                fprintf('Successfully downloaded and created: ''%s''.\n',...
                        destFullFile);
            end
        end
    end
end

