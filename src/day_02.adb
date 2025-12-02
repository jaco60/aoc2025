with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Containers.Vectors;
with GNAT.String_Split; use GNAT.String_Split;
with GNAT.Regpat;       use GNAT.Regpat;

procedure Day_02 is

   type Interval is record
      Start  : Long_Integer;
      Finish : Long_Integer;
   end record;

   package Interval_Vectors is new
     Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Interval);

   function Parse_Interval (S : String) return Interval is
      Subs   : Slice_Set;
      Result : Interval;
   begin
      -- Create (Subs, Trim (S, Ada.Strings.Both), "-");
      Create (Subs, S, "-");
      Result.Start := Long_Integer'Value (Slice (Subs, 1));
      Result.Finish := Long_Integer'Value (Slice (Subs, 2));

      return Result;
   end Parse_Interval;

   function Read_Input (File_Name : String) return Interval_Vectors.Vector is
      Input_File : File_Type;
      Intervals  : Interval_Vectors.Vector;
      Fields     : Slice_Set;
   begin
      -- Récupérer la ligne du fichier (une seule ligne attendue)
      Open (Input_File, In_File, File_Name);

      declare
         Line : constant String := Get_Line (Input_File);
      begin
         Close (Input_File);

         -- Séparer les intervalles
         Create (Fields, Line, ",");
      end;

      -- Ajouter les intervalles au vecteur
      for I in 1 .. Slice_Count (Fields) loop
         Intervals.Append (Parse_Interval (Slice (Fields, I)));
      end loop;

      return Intervals;
   end Read_Input;

   function Solve
     (Intervals : Interval_Vectors.Vector; Pattern : String)
      return Long_Integer
   is
      Matcher : constant Pattern_Matcher := Compile (Pattern);
      Res     : Long_Integer := 0;
   begin
      for I of Intervals loop
         for N in I.Start .. I.Finish loop
            declare
               N_Str : constant String :=
                 Long_Integer'Image (N) (2 .. Long_Integer'Image (N)'Last);
            begin
               if Match (Matcher, N_Str) then
                  Res := @ + N;
               end if;
            end;
         end loop;
      end loop;
      return Res;
   end Solve;

   -- Partie 1 : trouver les nombres avec une partie répétée exactement une fois
   function Part1 (Intervals : Interval_Vectors.Vector) return Long_Integer
   is (Solve (Intervals, "^(.+)\1$"));

   -- Partie 2 : trouver les nombres avec une partie répétée au moins une fois
   function Part2 (Intervals : Interval_Vectors.Vector) return Long_Integer
   is (Solve (Intervals, "^(.+)\1+$"));

   Intervals : constant Interval_Vectors.Vector :=
     Read_Input ("inputs/day02/input.txt");

begin
   Put_Line ("Part 1: " & Part1 (Intervals)'Img);
   Put_Line ("Part 2: " & Part2 (Intervals)'Img);
end Day_02;
