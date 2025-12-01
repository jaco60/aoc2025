with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package AOC_Utils is

   -- Types communs
   package Natural_Vectors is new
     Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Natural);

   package String_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Unbounded_String);

   -- Lecture de fichier
   function Read_Lines (File_Name : String) return String_Vectors.Vector;

   function Read_Numbers (File_Name : String) return Natural_Vectors.Vector;

   procedure Read_Two_Columns
     (File_Name : String;
      Left      : out Natural_Vectors.Vector;
      Right     : out Natural_Vectors.Vector);

   -- Parsing
   function Split
     (S : String; Separator : String := " ") return String_Vectors.Vector;

   -- Utilitaires
   procedure Print_Result (Part : Positive; Result : String);

   function Abs_Diff (A, B : Integer) return Natural;

end AOC_Utils;
