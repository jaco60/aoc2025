with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
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

   function Max (Data : String) return Character is
      Res : Character := '0';
   begin
      for C of Data loop
         if C > Res then
            Res := C;
         end if;
      end loop;
      return Res;
   end Max;

   -- Renvoie la séquence max de K caractères de Data
   function Max_Joltage (Data : String; K : Natural := 2) return String is
      Next     : Character;
      Next_Pos : Natural;
   begin
      if K = 0 then
         return "";
      end if;
      Next := Max (Data (Data'First .. Data'First + Data'Length - K));
      Next_Pos := Index (Data, String'(1 => Next));
      return (Next & Max_Joltage (Data (Next_Pos + 1 .. Data'Last), K - 1));
   end Max_Joltage;

   function Solve
     (Data : Str_Vectors.Vector; K : Positive := 2) return Long_Integer
   is
      Result : Long_Integer := 0;
      Max    : Long_Integer;
   begin
      for D of Data loop
         Max := 0;
         for C of Max_Joltage (To_String (D), K) loop
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
