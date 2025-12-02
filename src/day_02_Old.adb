-- with Ada.Exceptions;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with GNAT.String_Split;     use GNAT.String_Split;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Regpat;           use GNAT.Regpat;

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
      Create (Subs, Trim (S, Ada.Strings.Both), "-");
      
      Result.Start := Long_Integer'Value (Slice (Subs, 1));
      Result.Finish := Long_Integer'Value (Slice (Subs, 2));
      
      return Result;
   end Parse_Interval;

   function Read_Input (File_Name : String) return Interval_Vectors.Vector is
      Input_File : File_Type;
      Intervals  : Interval_Vectors.Vector;
      Line       : Unbounded_String;
      Fields     : Slice_Set;
   begin
      -- Récupérer la ligne du fichier
      Open (Input_File, In_File, File_Name);
      Line := To_Unbounded_String (Get_Line (Input_File));
      Close (Input_File);

      -- Séparer les intervalles
      Create (Fields, To_String (Line), ",");

      -- Ajouter les intervalles au vecteur
      for I in 1 .. Slice_Count (Fields) loop
         Intervals.Append (Parse_Interval (Slice (Fields, I)));
      end loop;

      return Intervals;
   end Read_Input;

   --  function Part1 (Intervals : Interval_Vectors.Vector) return Long_Integer is
   --     Res : Long_Integer := 0;
   --  begin
   --     for I of Intervals loop
   --        -- Traitement de chaque nombre de l'intervalle I
   --        for N in I.Start .. I.Finish loop
   --           declare
   --              N_Str : constant String :=
   --                Trim (Long_Integer'Image (N), Ada.Strings.Left);
   --           begin
   --              if N_Str'Length mod 2 = 0 then
   --                 if N_Str (1 .. N_Str'Length / 2)
   --                   = N_Str (N_Str'Length / 2 + 1 .. N_Str'Last)
   --                 then
   --                    Res := @ + N;
   --                 end if;
   --              end if;
   --           end;
   --        end loop;
   --        New_Line;
   --     end loop;
   --     return Res;
   --  end Part1;

   function Part1_Bis (Intervals : Interval_Vectors.Vector) return Long_Integer is
      Res     : Long_Integer := 0;
      Pattern : constant String := "^(.+)\1$";
      Matcher : constant Pattern_Matcher := Compile (Pattern);
   begin
      for I of Intervals loop
         -- Traitement de chaque nombre de l'intervalle I
         for N in I.Start .. I.Finish loop
            declare
               N_Str : constant String :=
                 Trim (Long_Integer'Image (N), Ada.Strings.Left);
            begin
               if Match (Matcher, N_Str) then
                  Res := @ + N;
               end if;
            end;
         end loop;
      end loop;
      return Res;
   end Part1_Bis;

   function Part2 (Intervals : Interval_Vectors.Vector) return Long_Integer is
      Res     : Long_Integer := 0;
      Pattern : constant String := "^(.+)\1+$";
      Matcher : constant Pattern_Matcher := Compile (Pattern);
   begin
      for I of Intervals loop
         -- Traitement de chaque nombre de l'intervalle I
         for N in I.Start .. I.Finish loop
            declare
               N_Str : constant String :=
                 Trim (Long_Integer'Image (N), Ada.Strings.Left);
            begin
               if Match (Matcher, N_Str) then
                  Res := @ + N;
               end if;
            end;
         end loop;
      end loop;
      return Res;
   end Part2;

   Intervals : constant Interval_Vectors.Vector :=
     Read_Input ("inputs/day02/input.txt");

begin
   Put_Line ("Part 1: " & Part1_Bis (Intervals)'Img);
   Put_Line ("Part 2: " & Part2 (Intervals)'Img);
end Day_02;
