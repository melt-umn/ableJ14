grammar edu:umn:cs:melt:ableJ14:driver:strict;

--import core ;
import edu:umn:cs:melt:ableJ14:concretesyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax;
--import silver:driver;

----------------------------------------------------------------------
-- Main driver function

function driver
IO ::= args::String io_in::IO extensionParser::(Root_C ::= String) hostParser::(Root_C ::= String) {

  local attribute commandLineFile :: String ;  
  commandLineFile = args ;

  local attribute classPath :: IOString;
  classPath = envVar ("JAVA_PATH", io_in);

  local attribute compileResult :: CompilationResult;
  compileResult = firstCompileFiles (classPath.io, commandLineFile, extensionParser, classPath.sValue, globalEnv);

  local attribute globalEnv :: [ ScopeEnv ];
  globalEnv = [ scopeEnv (-1, convertEnvItems (fullyQualifiedDefs, globalEnv)) ];

  local attribute fullyQualifiedDefs :: [ EnvItem ];
  fullyQualifiedDefs = convertDefsToFullyQualifiedDefs (compileResult.type_defs);

  local attribute compile_action :: IO ;
  compile_action = print("compile_action\n\n", if classPath.sValue != ""
		then compileResult.io 
		else error ("JAVA_PATH is not defined!"));
   
  return compile_action ;
}

-- driver function: separate function to generate the java translation of the command line argument
function firstCompileFiles
CompilationResult ::= io_in::IO commandLineFile::String extensionParser::(Root_C ::= String) classPath::String globalEnv::[ ScopeEnv ] {

 local attribute commandLineFileResult :: FileCompilationResult;
 commandLineFileResult = compileFile (io_in, commandLineFile, extensionParser, classPath, globalEnv);

 local attribute rest :: CompilationResult;
 rest = compileFiles (commandLineFileResult.io, [ commandLineFile ], files_to_compile, extensionParser, classPath, 
			commandLineFileResult.type_defs, globalEnv);

 return compilation_result (decorate file_io_action (commandLineFileResult.decoratedRoot) with {ioIn = rest.io;}.ioOut, 
				rest.needed_files, rest.type_defs);

 local attribute files_to_compile :: [ String ];
 files_to_compile = getNewFilesToCompile ([ commandLineFile ], removeDupes (commandLineFileResult.needed_files));
}

-- main driver function
function compileFiles
CompilationResult ::= io_in::IO files_compiled::[String] files_to_compile::[String] extensionParser::(Root_C ::= String) classPath::String 
			defs_so_far::[ EnvItem ] globalEnv::[ ScopeEnv ] {

 local attribute firstFile :: String;
 firstFile = head (files_to_compile);

 local attribute firstFileResult :: FileCompilationResult;
 firstFileResult = compileFile (io_in, firstFile, extensionParser, classPath, globalEnv);

 local attribute new_files_to_compile :: [ String ];
 new_files_to_compile = getNewFilesToCompile (files_compiled ++ files_to_compile, removeDupes (firstFileResult.needed_files));

-- !!! check for duplication of command line argument file, in the files in its package
 return if null (files_to_compile)
        then compilation_result (io_in, files_compiled, defs_so_far)
        else compileFiles (firstFileResult.io, files_compiled ++ [ firstFile ], tail (files_to_compile) ++ new_files_to_compile, extensionParser, classPath, 
				defs_so_far ++ firstFileResult.type_defs, globalEnv);
}

function getNewFilesToCompile
[ String ] ::= files_so_far::[ String ] new_needed_files::[ String ] {
 return
    if null (new_needed_files)
    then []
    else ((if memberString (head (new_needed_files), files_so_far)
           then []
           else [head (new_needed_files)]) ++ getNewFilesToCompile (files_so_far, tail (new_needed_files)));
}

inherited attribute importErrors :: [ Error ] occurs on Root;

function compileFile
FileCompilationResult ::= io_in::IO fileToCompile::String extensionParser::(Root_C ::= String) classPath::String globalEnv::[ ScopeEnv ] {

  -- checking for .defs file

  local attribute defsFileName :: String;
  defsFileName = substring (0, lastindexof (".", fileToCompile), fileToCompile) ++ ".defs";

  local attribute defsFileExists :: IOBoolean;
  defsFileExists = isFile (defsFileName, io_in);

  -- if .defs already exists

  local attribute defsFileRead :: IOString;
  defsFileRead = readFile (defsFileName, defsFileExists.io);

  local attribute defsText :: String;
  defsText = defsFileRead.sValue;

  local attribute defsInfo :: DefsFileInfo;
  defsInfo = extensionParser ("***" ++ defsFileName ++ "***" ++ defsText).ast_Root.defsFileInfo;

  -- if .defs file does not exist
  -- write to .defs file file to compile

  local attribute newDefsInfo :: DefsFileInfo;
  newDefsInfo = defs_file_info (neededImportedSingleTypes_ ++ neededCurrentPackageTypes_ ++ neededImportedOnDemandTypes_ ++ neededFullyQualifiedTypes_, r.type_defs,
					availableSingleTypeErrors.errors ++ availableCurrentPackageTypeErrors.errors ++ availableOnDemandTypeErrors.errors);

  local attribute defsFileWrite :: IO;
  defsFileWrite = writeFile (defsFileName, newDefsInfo.unparse, jextFileRead.io);

  -- read .jext file
  -- get ride of jextFileExists, or use it

  local attribute jextFileExists :: IOBoolean;
  jextFileExists = isFile (fileToCompile, defsFileExists.io);

  local attribute jextFileRead :: IOString;
  jextFileRead = readFile (fileToCompile, jextFileExists.io);

  local attribute jextText :: String;
  jextText = jextFileRead.sValue;

  local attribute fileDefs :: [ EnvItem ];
  fileDefs = if defsFileExists.bValue
	 	then case defsInfo of
			defs_file_info (_, typeDefs, _) -> typeDefs |
			_ -> error ("Internal compiler error 1 in function compileFile with file " ++ fileToCompile)
		     end
		else r.type_defs;

  local attribute r :: Root;
  r = extensionParser (jextText).ast_Root;
  r.file_name = fileToCompile;

  r.type_env = [ scopeEnv (-1, convertEnvItems (fileDefs, r.type_env)) ] ++ globalEnv;

  local attribute availableSingleTypeErrors :: LFQNs_Errors;
  availableSingleTypeErrors = getAvailableSingleTypes (r.singleImports, [ classPath ]);
  r.availableImportedSingleTypes = availableSingleTypeErrors.fullyQualifiedNames;

  local attribute availableCurrentPackageTypeErrors :: LFQNs_Errors;
  availableCurrentPackageTypeErrors = getAvailableCurrentPackageTypes (r.thisPackage_syn, fileToCompile, [ classPath ]);
  r.availableCurrentPackageTypes = availableCurrentPackageTypeErrors.fullyQualifiedNames;

  local attribute availableOnDemandTypeErrors :: LFQNs_Errors;
  availableOnDemandTypeErrors = getAvailableOnDemandTypes (r.onDemandImports, [ classPath ]);
  r.availableImportedOnDemandTypes = availableOnDemandTypeErrors.fullyQualifiedNames;

  local attribute availableFullyQualifiedTypeErrors :: LFQNs_Errors;
  availableFullyQualifiedTypeErrors = getFullyQualifiedTypes (r.neededFullyQualifiedTypes, [ classPath ]);
  r.availableFullyQualifiedTypes = availableFullyQualifiedTypeErrors.fullyQualifiedNames;

  local attribute neededImportedSingleTypes_ :: [ LFQN ];
  neededImportedSingleTypes_ = uniqueFullyQualifiedNames (r.neededImportedSingleTypes);
  local attribute neededImportedSingleFiles :: [ String ];
  neededImportedSingleFiles = getPackageFiles (classPath, neededImportedSingleTypes_);

  local attribute neededCurrentPackageTypes_ :: [ LFQN ];
  neededCurrentPackageTypes_ = uniqueFullyQualifiedNames (r.neededCurrentPackageTypes);
  local attribute neededCurrentPackageFiles :: [ String ];
  neededCurrentPackageFiles = getPackageFiles (classPath, neededCurrentPackageTypes_);

  local attribute neededImportedOnDemandTypes_ :: [ LFQN ];
  neededImportedOnDemandTypes_ = uniqueFullyQualifiedNames (r.neededImportedOnDemandTypes);
  local attribute neededImportedOnDemandFiles :: [ String ];
  neededImportedOnDemandFiles = getPackageFiles (classPath, neededImportedOnDemandTypes_);

  local attribute neededFullyQualifiedTypes_ :: [ LFQN ];
  neededFullyQualifiedTypes_ = availableFullyQualifiedTypeErrors.fullyQualifiedNames;
  local attribute neededFullyQualifiedFiles :: [ String ];
  neededFullyQualifiedFiles = getPackageFiles (classPath, neededFullyQualifiedTypes_);

  r.importErrors = availableSingleTypeErrors.errors ++ availableCurrentPackageTypeErrors.errors ++ availableOnDemandTypeErrors.errors;

  return if defsFileExists.bValue
	 then case defsInfo of
		defs_file_info (ts, typeDefs, errs) -> file_compilation_result (jextFileRead.io, getPackageFiles (classPath, ts), r, typeDefs) |
		_ -> error ("Internal compiler error 2 in function compileFile with file " ++ fileToCompile)
	      end

	 else file_compilation_result (defsFileWrite, neededImportedSingleFiles ++ neededCurrentPackageFiles ++ neededImportedOnDemandFiles ++ neededFullyQualifiedFiles, 
					r, r.type_defs);
}

function getPackageFiles
[ String ] ::= classPath::String neededTypes_::[ LFQN ] {
 return if null (neededTypes_)
	then []
	else (classPath ++ "/" ++ (head (neededTypes_)).fullyQualifiedName.qualifiedName ++ ".jext") :: getPackageFiles (classPath, tail (neededTypes_));
}


----------------------------------------------------------------------

-- Nonterminals and productions needed to implement tuples

nonterminal CompilationResult with io, needed_files, type_defs;
nonterminal FileCompilationResult with io, needed_files, decoratedRoot, type_defs;
synthesized attribute needed_files :: [ String ];
synthesized attribute decoratedRoot :: Decorated Root;

abstract production compilation_result
cr::CompilationResult ::= io_::IO needed_files_::[ String ] type_defs_::[ EnvItem ] {
  cr.io = io_;
  cr.needed_files = needed_files_;
  cr.type_defs = type_defs_;
}

abstract production file_compilation_result
cr::FileCompilationResult ::= io_::IO needed_files_::[ String ] decoratedRoot_::Decorated Root type_defs_::[ EnvItem ] {
  cr.io = io_; 
  cr.needed_files = needed_files_;
  cr.decoratedRoot = decoratedRoot_;
  cr.type_defs = type_defs_;
}

----------------------------------------------------------------------

-- Function to generate the base java code filename

function javaName
String ::= filename::String {
 return  if file_name_ext == "java"
         then base_file_name ++ "_pp.java"
         else base_file_name ++ ".java" ;

 local attribute base_file_name :: String ;
 base_file_name = substring (0, indexOf (".",filename), filename) ;

 local attribute file_name_ext :: String ;
 file_name_ext = substring (indexOf(".",filename) + 1, length (filename), filename) ;
}

----------------------------------------------------------------------

-- IO Stuff

nonterminal IO_Action with ioIn, ioOut ;

synthesized attribute ioOut :: IO;
inherited attribute ioIn :: IO;

abstract production io_action_write_file
task::IO_Action ::= fn::String text::String {
 task.ioOut = writeFile (fn, text, task.ioIn);
}

abstract production io_action_null
task::IO_Action ::= {
 task.ioOut = task.ioIn;
}

abstract production io_action_sequence
task::IO_Action ::= t1::IO_Action t2::IO_Action {
 task.ioOut = t2.ioOut ;
 t1.ioIn = task.ioIn ;
 t2.ioIn = t1.ioOut ;
}

-- add aspects to this production and add values to the analysisAction and translationAction attributes
-- to create further IO actions

abstract production file_io_action
top::IO_Action ::= r::Decorated Root {
  production attribute analysisAction :: IO_Action with io_action_sequence;
  analysisAction := if null (r.importErrors ++ r.errors)
			then io_action_null ()
			else error ("\nErrors\n: " ++ printErrors (r.importErrors ++ r.errors, r.file_name) ++ "\n");

  production attribute translationAction :: IO_Action with io_action_sequence;
  translationAction := if null (r.importErrors ++ r.errors) 
			then io_action_write_file (javaName (r.file_name), r.basepp)
			else io_action_null ();

  top.ioOut = decorate io_action_sequence (analysisAction, translationAction) with {ioIn = top.ioIn;}.ioOut;
}
