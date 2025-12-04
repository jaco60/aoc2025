with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;

procedure Day_04 is

   type Coordinates is record
      X, Y : Natural;
   end record;

   function Hash (C : Coordinates) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type'Mod (C.X * 65536 + C.Y);
   end Hash;

   package Str_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Unbounded_String);


   package Coord_Sets is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => Coordinates,
        Hash                => Hash,
        Equivalent_Elements => "=");

   use Coord_Sets;

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

   function To_Set (Data : Str_Vectors.Vector) return Set is
      Result : Set;
      Lig    : Natural := 0;
   begin
      for Line of Data loop
         Lig := @ + 1;
         for Col in To_String (Line)'Range loop
            if Element (Line, Col) = '@' then
               Result.Insert (Coordinates'(Lig, Col));
            end if;
         end loop;
      end loop;
      return Result;
   end To_Set;

   function Count_Adj (Data : Set; Coord : Coordinates) return Natural is
      Result : Natural := 0;
   begin
      for X in Coord.X - 1 .. Coord.X + 1 loop
         for Y in Coord.Y - 1 .. Coord.Y + 1 loop
            if Data.Contains (Coordinates'(X, Y)) then
               Result := @ + 1;
            end if;
         end loop;
      end loop;
      return (if Data.Contains (Coord) then Result - 1 else Result);
   end Count_Adj;

   function Part1 (Data : Set) return Natural is
      Result : Natural := 0;
   begin
      for Coord of Data loop
         if Count_Adj (Data, Coord) < 4 then
            Result := @ + 1;
         end if;
      end loop;
      return Result;
   end Part1;

   Data : constant Str_Vectors.Vector := Read_Lines ("inputs/day04/input.txt");

   Data_Set : constant Set := To_Set (Data);
begin
   Put_Line ("Part 1 : " & Part1 (Data_Set)'Img);
end Day_04;
