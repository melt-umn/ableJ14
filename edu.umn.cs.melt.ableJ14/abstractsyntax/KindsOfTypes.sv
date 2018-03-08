grammar edu:umn:cs:melt:ableJ14:abstractsyntax;

-- There are four "kinds" of type in the host language.
-- 1. Reference Types
-- 2. Primitive Types
-- 3. Interface Types
-- 4. An "Unknown" Type
-- 5. The Null Type

-- I'm adding
-- 6. Class Types
-- 7. Array Types
-- 8. Void Type

synthesized attribute isReferenceType :: Boolean ;
synthesized attribute isPrimitiveType :: Boolean ;
synthesized attribute isInterfaceType :: Boolean ;
synthesized attribute isUnknownType   :: Boolean ;
synthesized attribute isNullType      :: Boolean ;
synthesized attribute isClassType     :: Boolean ;
synthesized attribute isArrayType     :: Boolean ;
synthesized attribute isVoidType     :: Boolean ;

attribute isReferenceType occurs on TypeRep ;
attribute isPrimitiveType occurs on TypeRep ;
attribute isInterfaceType occurs on TypeRep ;
attribute isUnknownType   occurs on TypeRep ;
attribute isNullType      occurs on TypeRep ;
attribute isClassType     occurs on TypeRep ;
attribute isArrayType     occurs on TypeRep ;
attribute isVoidType     occurs on TypeRep ;

