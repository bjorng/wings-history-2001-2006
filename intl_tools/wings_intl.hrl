-compile({parse_transform,tools}).

-define(STR(A,B,Str), wings_lang:str({?MODULE,A,B},Str)).
-define(__(Key,Str), wings_lang:str({?MODULE,Key},Str)).
