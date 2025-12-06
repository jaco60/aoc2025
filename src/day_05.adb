with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;
with Ada.Containers;        use Ada.Containers;
with GNAT.String_Split;     use GNAT.String_Split;

procedure Day_05 is

   package Str_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Unbounded_String);

   type Interval is record
      Start, Stop : Long_Integer;
   end record;

   package Interval_Vectors is new
     Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Interval);

   package Int_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Long_Integer);

   type File_Content is record
      Ranges : Interval_Vectors.Vector;
      Ids    : Int_Vectors.Vector;
   end record;

   package Int_Sets is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => Long_Integer,
        Hash                => Ada.Containers.Hash_Type'Mod,
        Equivalent_Elements => "=");

   function Read_Lines (File_Name : String) return Str_Vectors.Vector is
      File   : File_Type;
      Result : Str_Vectors.Vector;
   begin
      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         Result.Append (To_Unbounded_String (Get_Line (File)));
      end loop;
      Close (File);
      return Result;
   end Read_Lines;

   function Parse_Interval (S : String) return Interval is
      Subs   : Slice_Set;
      Result : Interval;
   begin
      Create (Subs, S, "-");
      Result.Start := Long_Integer'Value (Slice (Subs, 1));
      Result.Stop := Long_Integer'Value (Slice (Subs, 2));

      return Result;
   end Parse_Interval;

   function Split_Lines (Lines : Str_Vectors.Vector) return File_Content is
      Result : File_Content;
      I      : Positive := Lines.First_Index;
   begin
      while Lines.Element (I) /= "" loop
         declare
            Str_Interval : constant String := To_String (Lines.Element (I));
         begin
            Result.Ranges.Append (Parse_Interval (Str_Interval));
            I := @ + 1;
         end;
      end loop;
      for J in I + 1 .. Lines.Last_Index loop
         Result.Ids.Append
           (Long_Integer'Value (To_String (Lines.Element (J))));
      end loop;
      return Result;
   end Split_Lines;

   function Part1 (Lines : File_Content) return Count_type is
      Result : Count_Type := 0;
   begin
      for Id of Lines.Ids loop
         declare
            Rng : constant Interval_Vectors.Vector := Lines.Ranges;
         begin
            Ranges :
            for R of Rng loop
               if Id in R.Start .. R.Stop then
                  Result := @ + 1;
                  exit Ranges;
               end if;
            end loop Ranges;
         end;
      end loop;
      return Result;
   end Part1;

   -- Not mine...
   function Part2 (Lines : File_Content) return Long_Integer is
      -- Copier et trier les intervalles
      Sorted_Ranges : Interval_Vectors.Vector := Lines.Ranges;

      -- Fonction de comparaison pour le tri
      function "<" (Left, Right : Interval) return Boolean
      is (Left.Start < Right.Start);

      package Interval_Sorting is new Interval_Vectors.Generic_Sorting;

      Total : Long_Integer := 0;
   begin
      if Sorted_Ranges.Is_Empty then
         return 0;
      end if;

      -- Trier les intervalles par ordre croissant de Start
      Interval_Sorting.Sort (Sorted_Ranges);

      -- Fusionner les intervalles qui se chevauchent
      declare
         Current_Start : Long_Integer := Sorted_Ranges.First_Element.Start;
         Current_Stop  : Long_Integer := Sorted_Ranges.First_Element.Stop;
      begin
         for I in Sorted_Ranges.First_Index + 1 .. Sorted_Ranges.Last_Index
         loop
            declare
               R : constant Interval := Sorted_Ranges.Element (I);
            begin
               if R.Start <= Current_Stop + 1 then
                  -- Chevauchement ou adjacence : fusionner
                  Current_Stop := Long_Integer'Max (Current_Stop, R.Stop);
               else
                  -- Pas de chevauchement : comptabiliser l'intervalle fusionné
                  Total := Total + (Current_Stop - Current_Start + 1);
                  Current_Start := R.Start;
                  Current_Stop := R.Stop;
               end if;
            end;
         end loop;

         -- Comptabiliser le dernier intervalle
         Total := Total + (Current_Stop - Current_Start + 1);
      end;

      return Total;
   end Part2;

   Lines : constant File_Content :=
     Split_Lines (Read_Lines ("inputs/day05/sample.txt"));

begin
   Put_Line ("Part 1 : " & Part1 (Lines)'Img);
   Put_Line ("Part 2 : " & Part2 (Lines)'Img);
end Day_05;
