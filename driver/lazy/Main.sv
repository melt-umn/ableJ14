grammar edu:umn:cs:melt:ableJ14:driver:lazy;

import core;
import edu:umn:cs:melt:ableJ14:concretesyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax;
import silver:driver;

----------------------------------------------------------------------
-- Main driver function

function driver
IO ::= args::String io_in::IO extensionParser::Production (Root_C ::= String) hostParser::Production (Root_C ::= String) {

  local attribute commandLineFile :: String ;  
  commandLineFile = args;
  
  local attribute classPath :: IOString;
  classPath = envVar ("JAVA_PATH", io_in);

  local attribute currentDirectory :: IOString;
  currentDirectory = cwd (classPath.io);

  local attribute allNeededTypesRoots :: [ LFQN_DecoratedRoot_Defs ];
  allNeededTypesRoots = getNeededTypesStartingWithCommandLine (commandLineFile, extensionParser, hostParser, getDirectoriesFromClassPath (classPath.sValue), currentDirectory.sValue, globalEnv);

  -- this is the lazy environment
  local attribute globalEnv :: [ ScopeEnv ];
  globalEnv = [ scopeEnv (-1, mkEnv (allNeededTypesRoots, globalEnv)) ];

  -- The following expression decides which errors are evaluated. 
  -- If we wish, we can cause errors to be evaluated for *all* neededtypes 
  -- (there's a few hundred of them)
  -- For now, only the command line file's errors are evaluated and only the command line file's basepp is printed out.
  -- This is what makes the lazy environment useful.

  local attribute commandLineRoot :: Decorated Root;
  commandLineRoot = head (allNeededTypesRoots).decoratedRoot;

  local attribute compile_action :: IO ;
  compile_action = if classPath.sValue != ""
          	         then decorate file_io_action (commandLineRoot) with {ioIn = currentDirectory.io ;}.ioOut 
                         else error ("JAVA_PATH is not defined!");

  return
     if   ! (is_jext_file(commandLineFile) || is_java_file (commandLineFile))-- is not a .jext or .java file
     then error ("\nError: - file \"" ++ commandLineFile ++ "\" must have .jext or .java suffix.\n")
     else compile_action ;
}


----------------------------------------------------------------------
-- A function to parse a file - used only when the --parseonly flag is provided.

function parse_only
IO ::= parse_io::IO filename::String extensionParser::Production(Root_C ::= String) hostParser::Production (Root_C ::= String) 
{
  local attribute isF :: IOBoolean;
  isF = isFile(filename, parse_io);

  local attribute file :: IOString;
  file = readFile(filename, isF.io);

  local attribute text :: String;
  text = if isF.bValue then file.sValue else "" ;
  
  local attribute r :: Root_C ;
  r = if is_jext_file (filename)
	then extensionParser (text)
      else if is_java_file (filename)
	then hostParser (text)
      else error ("Internal compiler error in parse_only");

  local attribute print_pp :: IO ;
  print_pp = print ("parsing of \"" ++ filename ++ "\" is:\n" ++
                    "------------------------------------------------------------\n" ++
                    r.canparse ++
                    "\n\n\n", 
                    file.io );
  return if isF.bValue then print_pp else error ("File \"" ++ filename ++ "\" not found\n\n" ) ;
}

----------------------------------------------------------------------

-- A lazy definition for the list of neededTypes and import-related errors

function getNeededTypes
[ LFQN_DecoratedRoot_Defs ]::= soFar::[ LFQN ] toDo::[ LFQN ] extensionParser::Production (Root_C ::= String) hostParser::Production (Root_C ::= String) classPathDirectories::[ String ] currentDirectory::String globalEnv::[ ScopeEnv ] {

  local attribute firstToDo :: LFQN;
  firstToDo = head (toDo);

  local attribute firstResult :: LFQNs_DecoratedRoot_Defs;
  firstResult = getNeededTypesForOneFile (firstToDo, extensionParser, hostParser, classPathDirectories, currentDirectory, globalEnv);

  return if null (toDo)
		then []
	 else if memberLFQN (firstToDo, soFar)
		then getNeededTypes (soFar, tail (toDo), extensionParser, hostParser, classPathDirectories, currentDirectory, globalEnv)
	 else lfqn_decorated_root_defs (firstToDo, firstResult.decoratedRoot, firstResult.type_defs)
		:: getNeededTypes (soFar ++ [ firstToDo ], tail (toDo) ++ firstResult.neededTypes, extensionParser, hostParser, classPathDirectories, currentDirectory, globalEnv);
}

function getNeededTypesStartingWithCommandLine
[ LFQN_DecoratedRoot_Defs ] ::= commandLineFile::String extensionParser::Production (Root_C ::= String) hostParser::Production (Root_C ::= String) classPathDirectories::[ String ] currentDirectory::String globalEnv::[ ScopeEnv ] {

  local attribute commandLineFileResult :: CommandLineLFQNs_DecoratedRoot;
  commandLineFileResult = getNeededTypesForCommandLineFile (commandLineFile, extensionParser, hostParser, classPathDirectories, currentDirectory, globalEnv);

  return lfqn_decorated_root_defs (if commandLineFileResult.isInPackage then commandLineFileResult.L_FQN else lfqn ("DummyPackage", fully_qualified_name_none ()),
				    commandLineFileResult.decoratedRoot, commandLineFileResult.decoratedRoot.type_defs)
		:: getNeededTypes (if commandLineFileResult.isInPackage then [ commandLineFileResult.L_FQN ] else [], 
					 commandLineFileResult.neededTypes, extensionParser, hostParser, classPathDirectories, currentDirectory, globalEnv);
}

inherited attribute importErrors :: [ Error ] occurs on Root;

-- the function reads in the file and gets the list of needed files, errors and type defs, and writes
-- the list of needed files, the type defs and import-related errors to a .defs file
function getNeededTypesForOneFile
LFQNs_DecoratedRoot_Defs ::= T::LFQN extensionParser::Production (Root_C ::= String) hostParser::Production (Root_C ::= String) classPathDirectories::[ String ] currentDirectory::String globalEnv::[ ScopeEnv ] {

  -- first check to see if a .defs file exists and is nonempty, if it does, we get the info from it

  local attribute defsFileName :: String;
  defsFileName = T.location ++ "/" ++ T.fullyQualifiedName.qualifiedFileName ++ ".defs";

  local attribute defsFileExists :: IOBoolean;
  defsFileExists = isFile (defsFileName, unsafeio);

  -- if .defs file exists, the following are forced by the return statement
  local attribute oldDefsFileText :: String;
  oldDefsFileText = readFile (defsFileName, defsFileExists.io).sValue;

  local attribute oldDefsFileInfo :: DefsFileInfo;
  oldDefsFileInfo = if jextFileExists.bValue
			then extensionParser ("***" ++ defsFileName ++ "***" ++ oldDefsFileText).ast_Root.defsFileInfo
		    else if javaFileExists.bValue
			then hostParser ("***" ++ defsFileName ++ "***" ++ oldDefsFileText).ast_Root.defsFileInfo
		    else error ("Internal compiler error: Neither " ++ jextFileName ++ " nor " ++ javaFileName ++ " exists!");

  local attribute defsFileValid :: Boolean;
  defsFileValid = defsFileExists.bValue && oldDefsFileText != "";

  -- if .defs file does not exist or is empty, the following are forced by the return statement
  -- reading in the .jext or .java file
  local attribute jextFileName :: String;
  jextFileName = T.location ++ "/" ++ T.fullyQualifiedName.qualifiedFileName ++ ".jext";

  local attribute jextFileExists :: IOBoolean;
  jextFileExists = isFile (jextFileName, defsFileExists.io);

  local attribute javaFileName :: String;
  javaFileName = T.location ++ "/" ++ T.fullyQualifiedName.qualifiedFileName ++ ".java";

  local attribute javaFileExists :: IOBoolean;
  javaFileExists = isFile (javaFileName, defsFileExists.io);

  local attribute javaFileRead :: IOString;
  javaFileRead = if jextFileExists.bValue
			then readFile (jextFileName, jextFileExists.io)
		 else if javaFileExists.bValue
			then readFile (javaFileName, javaFileExists.io)
		 else error ("Internal compiler error: Neither " ++ jextFileName ++ " nor " ++ javaFileName ++ " exists!");

  -- parsing the file and constructing the AST
  local attribute r :: Root;
  r = if jextFileExists.bValue
	then extensionParser (javaFileRead.sValue).ast_Root
      else if javaFileExists.bValue
	then hostParser (javaFileRead.sValue).ast_Root
      else error ("Internal compiler error: Neither " ++ jextFileName ++ " nor " ++ javaFileName ++ " exists!");

  local attribute correctFileName :: String;
  correctFileName = if jextFileExists.bValue
			then jextFileName
		    else if javaFileExists.bValue
			then javaFileName
		    else error ("Internal compiler error: Neither " ++ jextFileName ++ " nor " ++ javaFileName ++ " exists!");

  r.file_name = correctFileName;

  local attribute availableSingleTypeErrors :: LFQNs_Errors;
  availableSingleTypeErrors = getAvailableSingleTypes (r.singleImports, classPathDirectories);
  r.availableImportedSingleTypes = availableSingleTypeErrors.L_FQNs;

  local attribute availableCurrentPackageTypeErrors :: LFQNs_Errors;
  availableCurrentPackageTypeErrors = getAvailableCurrentPackageTypes (r.thisPackage_syn, classPathDirectories, currentDirectory, correctFileName);
  r.availableCurrentPackageTypes = availableCurrentPackageTypeErrors.L_FQNs;

  local attribute availableOnDemandTypeErrors :: LFQNs_Errors;
  availableOnDemandTypeErrors = getAvailableOnDemandTypes (r.onDemandImports, classPathDirectories);
  r.availableImportedOnDemandTypes = availableOnDemandTypeErrors.L_FQNs;

  local attribute availableFullyQualifiedTypeErrors :: LFQNs_Errors;
  availableFullyQualifiedTypeErrors = getFullyQualifiedTypes (r.neededFullyQualifiedTypes, classPathDirectories );
  r.availableFullyQualifiedTypes = availableFullyQualifiedTypeErrors.L_FQNs;

  local attribute neededTypes_ :: [ LFQN ];
  neededTypes_ = uniqueLFQNs (r.neededImportedSingleTypes ++ r.neededCurrentPackageTypes ++ r.neededImportedOnDemandTypes ++ 
						availableFullyQualifiedTypeErrors.L_FQNs);

  -- writing the list of needed types, type defs and import-related errors to a .defs file
  local attribute newDefsFileText :: String;
  newDefsFileText = defs_file_info (neededTypes_, r.type_defs, 
			availableSingleTypeErrors.errors ++ availableCurrentPackageTypeErrors.errors ++ availableOnDemandTypeErrors.errors).unparse;

  local attribute defsFileWrite :: IO;
  defsFileWrite = writeFile (defsFileName, newDefsFileText, javaFileRead.io);

  -- now we force the .defs write by returning a list of neededtypes read from the .defs file
  local attribute defsFileRetrieve :: String;
  defsFileRetrieve = readFile (defsFileName, defsFileWrite).sValue;

  local attribute retrievedDefsFileInfo :: DefsFileInfo;
  retrievedDefsFileInfo = if jextFileExists.bValue
				then extensionParser (defsFileRetrieve).ast_Root.defsFileInfo
			  else if javaFileExists.bValue
				then hostParser (defsFileRetrieve).ast_Root.defsFileInfo
			  else error ("Internal compiler error: Neither " ++ jextFileName ++ " nor " ++ javaFileName ++ " exists!");

  -- Should we cache r.errors? Otherwise it makes no sense to check for .defs files
  r.type_env = globalEnv;
  r.importErrors = if defsFileValid then oldDefsFileInfo.errors else retrievedDefsFileInfo.errors;

  -- the return forces one of the two sets of attribute evaluations above
  return if defsFileValid
		then lfqns_decorated_root_defs (oldDefsFileInfo.neededTypes, r, oldDefsFileInfo.type_defs)
		else lfqns_decorated_root_defs (retrievedDefsFileInfo.neededTypes, r, retrievedDefsFileInfo.type_defs);
}

-- This does the same as getNeededTypesForOneFile
-- but there are a few things we have to do differently when handling the command line file.
-- We need to find out if the commandLineFile has a package. If it does, then it's included
-- in the list of needed types. Otherwise it's not, its defintions are handled separately.
-- No other type will need it, so we don't have to worry about duplicates.

-- The only issue is that we don't use the command line file's .defs file
-- since we have to parse the .java file to see if it has a package.
-- This can be fixed if we include this information in the .defs file.

-- We also need to generate the .java file for the command line file.

-- Finally, this won't work if the command line file is not in the current directory
-- i.e., you can't do ejc ../../T1.java.
-- I'll fix this when I separate out the different classes into their own files.
function getNeededTypesForCommandLineFile
CommandLineLFQNs_DecoratedRoot ::= commandLineFile::String extensionParser::Production (Root_C ::= String) hostParser::Production (Root_C ::= String) classPathDirectories::[ String ] currentDirectory::String globalEnv::[ ScopeEnv ] {

  local attribute javaFileRead :: IOString;
  javaFileRead = readFile (commandLineFile, unsafeio);

  -- parsing the file and constructing the AST
  local attribute r :: Root;
  r = if is_jext_file (commandLineFile)
	then extensionParser (javaFileRead.sValue).ast_Root
      else if is_java_file (commandLineFile)
	then hostParser (javaFileRead.sValue).ast_Root
      else error ("Internal compiler error in getNeededTypesForCommandLineFile");

  r.file_name = commandLineFile;

  local attribute availableSingleTypeErrors :: LFQNs_Errors;
  availableSingleTypeErrors = getAvailableSingleTypes (r.singleImports, classPathDirectories);
  r.availableImportedSingleTypes = availableSingleTypeErrors.L_FQNs;

  local attribute availableCurrentPackageTypeErrors :: LFQNs_Errors;
-- correctFileName here should have only the type name, no dots or anything else
  availableCurrentPackageTypeErrors = getAvailableCurrentPackageTypes (r.thisPackage_syn, classPathDirectories, currentDirectory, commandLineFile);
  r.availableCurrentPackageTypes = availableCurrentPackageTypeErrors.L_FQNs;

  local attribute availableOnDemandTypeErrors :: LFQNs_Errors;
  availableOnDemandTypeErrors = getAvailableOnDemandTypes (r.onDemandImports, classPathDirectories);
  r.availableImportedOnDemandTypes = availableOnDemandTypeErrors.L_FQNs;

  local attribute availableFullyQualifiedTypeErrors :: LFQNs_Errors;
  availableFullyQualifiedTypeErrors = getFullyQualifiedTypes (r.neededFullyQualifiedTypes, classPathDirectories);
  r.availableFullyQualifiedTypes = availableFullyQualifiedTypeErrors.L_FQNs;

  -- if the command line file is in a package, we include it in the list of needed types, see above.
  local attribute neededTypes_ :: [ LFQN ];
  neededTypes_ = uniqueLFQNs (r.neededImportedSingleTypes ++ r.neededCurrentPackageTypes ++ r.neededImportedOnDemandTypes ++ 
						availableFullyQualifiedTypeErrors.L_FQNs);

  -- the env items in *this* compilation unit are converted and passed to root separately
  -- if *this* compilation unit has only a default package, i.e., its env items are not 
  -- added to the global environment

  local attribute thisCompilationUnitConvertedItems :: [ EnvItem ];
  thisCompilationUnitConvertedItems = convertEnvItems (r.type_defs, thisCompilationUnitEnv);

  local attribute thisCompilationUnitEnv :: [ ScopeEnv ];
  thisCompilationUnitEnv = appendWithinScope (thisCompilationUnitConvertedItems, globalEnv);

  r.type_env = case r.thisPackage_syn of
		  fully_qualified_name_none () -> thisCompilationUnitEnv |
		  _ -> globalEnv
	       end;
  r.importErrors = availableSingleTypeErrors.errors ++ availableCurrentPackageTypeErrors.errors ++ availableOnDemandTypeErrors.errors;

  -- the return forces one of the two sets of attribute evaluations above
  return command_line_lfqns_decorated_root (
		neededTypes_, 
		r,
		case r.thisPackage_syn of
			fully_qualified_name_none () -> false |
			fqn -> true
		end,
		case r.thisPackage_syn of
			fully_qualified_name_none () -> lfqn ("DummyPackage", fully_qualified_name_unknown ()) |
 -- get rid of dots and get right location
			fqn -> lfqn (getPackageLocation (currentDirectory, commandLineFile, fqn), getQualifiedFQN (fqn, substring (0, lastindexof (".", commandLineFile), commandLineFile)))
		end);
}


----------------------------------------------------------------------

-- Lazy definition for the environment, the environment is passed to itself as a pointer

function mkEnv
[ EnvItem ] ::= lfqns::[ LFQN_DecoratedRoot_Defs ] globalEnv::[ ScopeEnv ] {
  return if null (lfqns)
		then []
		else (case head (lfqns).L_FQN.fullyQualifiedName of
			fully_qualified_name_none () -> [ :: EnvItem ] |
			fqn -> [ envItem (fqn.qualifiedName, fqn, getDclRep (head (lfqns).type_defs, globalEnv)) ]
		      end)
			++ mkEnv (tail (lfqns), globalEnv);
}

-- getDclRep returns a dclrep of the required type
-- for now, it just returns what's in .defs after "converting" it to a version
-- that contains globalEnv as a pointer
-- for e.g. class_type_rep_defs will be converted to class_type_rep, which takes
-- globalEnv as a parameter and uses it to define its fields attributes

-- an improvement would be to find *all* the members of a class and cache the info in
-- a .full file.
-- preliminary code for this is available in the .full files

-- this assumes that length (type_defs_) == 1, which it will be, once we separate out
-- the classes from the same compilation unit. For now, compilation units with multiple
-- classes won't work.
function getDclRep
DclRep ::= type_defs_::[ EnvItem ] globalEnv::[ ScopeEnv ] {
  return head (convertEnvItems (type_defs_, globalEnv)).dclrep;
}

----------------------------------------------------------------------

-- Nonterminals and productions needed to implement tuples

nonterminal LFQN_DecoratedRoot_Defs with L_FQN, decoratedRoot, type_defs;
synthesized attribute decoratedRoot :: Decorated Root;

abstract production lfqn_decorated_root_defs
top::LFQN_DecoratedRoot_Defs ::= L_FQN_::LFQN decoratedRoot_::Decorated Root type_defs_::[ EnvItem ] {
  top.L_FQN = L_FQN_;
  top.decoratedRoot = decoratedRoot_;
  top.type_defs = type_defs_;
}

nonterminal LFQNs_DecoratedRoot_Defs with neededTypes, decoratedRoot, type_defs;

abstract production lfqns_decorated_root_defs
top::LFQNs_DecoratedRoot_Defs ::= neededTypes_::[ LFQN ] decoratedRoot_::Decorated Root type_defs_::[ EnvItem ] {
  top.neededTypes = neededTypes_;
  top.decoratedRoot = decoratedRoot_;
  top.type_defs = type_defs_;
}

synthesized attribute isInPackage :: Boolean;

-- command line's defs are cached, they're taken off the root
nonterminal CommandLineLFQNs_DecoratedRoot with neededTypes, decoratedRoot, isInPackage, L_FQN;

abstract production command_line_lfqns_decorated_root
top::CommandLineLFQNs_DecoratedRoot ::= neededTypes_::[ LFQN ] decoratedRoot_::Decorated Root isInPackage_::Boolean L_FQN_::LFQN {
  top.neededTypes = neededTypes_;
  top.decoratedRoot = decoratedRoot_;
  top.isInPackage = isInPackage_;
  top.L_FQN = L_FQN_;
}

----------------------------------------------------------------------

-- Function to generate the base java code filename

-- fix dots
function javaName
String ::= filename::String {
 return  if file_name_ext == "java"
         then base_file_name ++ "_pp.java"
         else base_file_name ++ ".java" ;

 local attribute base_file_name :: String ;
 base_file_name = -- substring (0, indexOf (".",filename), filename) ;
                  basefilename(filename);

 local attribute file_name_ext :: String ;
 file_name_ext = substring (indexOf(".",filename) + 1, length (filename), filename) ;
}



function basefilename
String ::= filename::String
{
 return  base_file_name ;

 local attribute base_file_name :: String ;
 base_file_name = substring(0, dot_position, filename) ;

 -- The nonsense below is needed to handle filenames that have a dot before
 -- the suffix dot, as in "../demos/hello.xpml"
 -- There is no String reverse function, or lastIndexOf function to do this
 -- properly.

 local attribute dot_position :: Integer ;
 dot_position =      if substring(l-2, l-1, filename) == "." then l-2
                else if substring(l-3, l-2, filename) == "." then l-3
                else if substring(l-4, l-3, filename) == "." then l-4
                else if substring(l-5, l-4, filename) == "." then l-5
                else if substring(l-6, l-5, filename) == "." then l-6
                else 0 ;

 local attribute l :: Integer ;
 l = length(filename);
}

function is_jext_file
Boolean ::= filename::String
{ return file_name_ext == ".jext" ;

  local attribute file_name_ext :: String ;
  file_name_ext = substring(length(filename) - 5, length(filename), filename) ;
}

function is_java_file
Boolean ::= filename::String
{ return file_name_ext == ".java" ;

  local attribute file_name_ext :: String ;
  file_name_ext = substring(length(filename) - 5, length(filename), filename) ;
}

----------------------------------------------------------------------

-- IO Stuff

nonterminal IO_Action with ioIn, ioOut ;

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
			else error ("\n" ++ printErrors (r.importErrors ++ r.errors, r.file_name) ++ "\n");

  production attribute translationAction :: IO_Action with io_action_sequence;
  translationAction := if null (r.importErrors ++ r.errors) 
			then io_action_write_file (javaName (r.file_name), r.basepp)
			else io_action_null ();

  top.ioOut = decorate io_action_sequence (analysisAction, translationAction) with {ioIn = top.ioIn;}.ioOut;
}

-- this function attempts to find the right absolute path for the current compilation unit's package
-- e.g. the following should all return (in effect) "/a/b"
-- 	getPackageLocation ("/a/b/pkg", "Main.jext", pkg)
-- 	getPackageLocation ("/a/b", "pkg/Main.jext", pkg)
--      getPackageLocation ("/a/b/pkg/temp", "../Main.jext", pkg)
--      getPackageLocation ("/a/b/pkg/temp", "../../pkg/Main.jext", pkg)
function getPackageLocation
String ::= currentDirectory::String commandLineFile::String thisPack::FullyQualifiedName {
  return currentDirectory ++ "/../";
}

-- this function takes a strict JAVA_PATH (which is a list of colon-separated directories)
-- and generates a list of Strings
function getDirectoriesFromClassPath
[ String ] ::= classPath::String {

  local attribute firstColonPosition :: Integer;
  firstColonPosition = indexOf (":", classPath);

  return if length (classPath) == 0
		then []
	 else if firstColonPosition == -1
		then [ classPath ]
	 else if firstColonPosition == 0
		then getDirectoriesFromClassPath (substring (1, length (classPath), classPath))
	 else substring (0, firstColonPosition, classPath) ::
		getDirectoriesFromClassPath (substring (firstColonPosition + 1, length (classPath), classPath));
}
