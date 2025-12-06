with Ada.Containers;        use Ada.Containers;
with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with GNAT.String_Split;     use GNAT.String_Split;

procedure Day_06 is

   package Str_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Unbounded_String);

   package Int_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Long_Integer);
   use Int_Vectors;

   package Array_Int_Vectors is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => Int_Vectors.Vector);

   type File_Content is record
      Numbers : Array_Int_Vectors.Vector;
      Ops     : Unbounded_String;
   end record;


   function Read_Lines_Trimed (File_Name : String) return Str_Vectors.Vector is
      File  : File_Type;
      Lines : Str_Vectors.Vector;
   begin
      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         declare
            Untrimed : constant Unbounded_String :=
              To_Unbounded_String (Get_Line (File));
            Trimed   : constant Unbounded_String :=
              Trim (Untrimed, Ada.Strings.Both);
         begin
            Lines.Append (Trimed);
         end;
      end loop;
      Close (File);
      return Lines;
   end Read_Lines_Trimed;

   function Transpose
     (Lines : Str_Vectors.Vector) return Array_Int_Vectors.Vector
   is
      Horizontal_Numbers, Vertical_Numbers : Array_Int_Vectors.Vector;
      Nb_Cols                              : Positive;
   begin
      for Num in Lines.First_Index .. Lines.Last_Index - 1 loop
         -- Construction des vecteurs de nombres du fichier d'entrée
         declare
            Horiz_Line : Int_Vectors.Vector;
            Subs       : Slice_Set;
            Line       : constant Unbounded_String := Lines (Num);
         begin
            Create (Subs, To_String (Line), " ", Multiple);
            for I in 1 .. Slice_Count (Subs) loop
               Horiz_Line.Append (Long_Integer'Value (Slice (Subs, I)));
            end loop;
            Horizontal_Numbers.Append (Horiz_Line);
         end;
      end loop;

      -- Construction des vecteurs de nombres verticaux
      Nb_Cols := Positive (Horizontal_Numbers.Element (1).Length);

      for I in 1 .. Nb_Cols loop
         declare
            Vert_Line : Int_Vectors.Vector;
         begin
            for Row of Horizontal_Numbers loop
               declare
                  Element : constant Long_Integer := Row.Element (I);
               begin
                  Vert_Line.Append (Element);
               end;
            end loop;
            Vertical_Numbers.Append (Vert_Line);
         end;
      end loop;

      return Vertical_Numbers;
   end Transpose;

   function Calc_Operations
     (Numbers : Array_Int_Vectors.Vector; Opers : Unbounded_String)
      return Long_Integer
   is
      Total    : Long_Integer := 0;
      Subs     : Slice_Set;
      Num_Oper : Long_Integer := 1;

   begin
      Create (Subs, To_String (Opers), " ", Multiple);

      for Each_Seq_Of_Number of Numbers loop
         declare
            Result : Long_Integer;
         begin
            if Slice (Subs, Slice_Number (Num_Oper)) = "+" then
               Result := 0;
               for Num of Each_Seq_Of_Number loop
                  Result := @ + Num;
               end loop;
            else
               Result := 1;
               for Num of Each_Seq_Of_Number loop
                  Result := @ * Num;
               end loop;
            end if;
            Total := @ + Result;
            Num_Oper := @ + 1;
         end;
      end loop;
      return Total;
   end Calc_Operations;

   function Part_1 (Data : Str_Vectors.Vector) return Long_Integer is
      Numbers : constant Array_Int_Vectors.Vector := Transpose (Data);
      Opers   : constant Unbounded_String := Data.Last_Element;
   begin
      return Calc_Operations (Numbers, Opers);
   end Part_1;

   Data : constant Str_Vectors.Vector :=
     Read_Lines_Trimed ("inputs/day06/input.txt");

begin
   Put_Line ("Part 1 : " & Part_1 (Data)'Img);

end Day_06;
