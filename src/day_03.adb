with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Day_03 is

   package Str_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Unbounded_String);

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

   function Max_Joltage_Seq (Data : String; Nb_Digits : Positive) return String
   is
      Result    : Unbounded_String;
      Start_Idx : Positive := Data'First;
      End_Idx   : Positive;
      Max_Idx   : Positive;
      Max_Char  : Character;
   begin
      for Pos_Seq in 1 .. Nb_Digits loop
         End_Idx := Data'Length - (Nb_Digits - Pos_Seq);
         Max_Char := Data (Start_Idx);
         Max_Idx := Start_Idx;
         for Current_Idx in Start_Idx + 1 .. End_Idx loop
            declare
               Current_Char : constant Character := Data (Current_Idx);
            begin
               if Current_Char > Max_Char then
                  Max_Char := Current_Char;
                  Max_Idx := Current_Idx;
                  if Max_Char = '9' then
                     exit;
                  end if;
               end if;
            end;
         end loop;
         Append (Result, Max_Char);
         Start_Idx := Max_Idx + 1;
      end loop;
      return To_String (Result);
   end Max_Joltage_Seq;

   function Solve
     (Data : Str_Vectors.Vector; K : Positive := 2) return Long_Integer
   is
      Result : Long_Integer := 0;
      Max    : Long_Integer;
   begin
      for D of Data loop
         Max := 0;
         for C of Max_Joltage_Seq (To_String (D), K) loop
            Max :=
              @ * 10 + Long_Integer (Character'Pos (C) - Character'Pos ('0'));
         end loop;
         Result := @ + Max;
      end loop;
      return Result;
   end Solve;

   Data : constant Str_Vectors.Vector := Read_Lines ("inputs/day03/input.txt");

begin
   Put_Line ("Part1 : " & Solve (Data)'Img);
   Put_Line ("Part2 : " & Solve (Data, 12)'Img);
end Day_03;
