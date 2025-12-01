with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Exceptions;

procedure Day_01 is

   type Instruction is record
      Direction : Integer;  -- '1' (droite) ou '-1' (gauche)
      Steps     : Natural;
   end record;

   package Inst_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Instruction);

   Circle_Size : constant := 100;  -- Le compteur va de 0 a 99
   Start_Pos   : constant := 50;   -- Position initiale du compteur

   function Read_Input (File_Name : String) return Inst_Vectors.Vector is
      Input_File   : File_Type;
      Instructions : Inst_Vectors.Vector;
   begin
      Open (Input_File, In_File, File_Name);

      begin
         while not End_Of_File (Input_File) loop
            declare
               Line : constant String := Get_Line (Input_File);
               Dir  : Integer;
               Num  : Natural;
            begin
               if Line'Length < 2 then
                  raise Constraint_Error with "Invalid instruction length";
               end if;
               Dir := (if Line (1) = 'R' then 1 else -1);
               Num := Natural'Value (Line (2 .. Line'Last));
               Instructions.Append (Instruction'(Dir, Num));
            end;
         end loop;
      exception
         when Constraint_Error =>
            if Is_Open (Input_File) then
               Close (Input_File);
            end if;
            raise;
      end;

      Close (Input_File);
      return Instructions;
   end Read_Input;

   function Part1 (Instructions : Inst_Vectors.Vector) return Natural is
      Start : Natural := Start_Pos;
      Count : Natural := 0;
   begin
      for Inst of Instructions loop
         begin
            Start := (Start + Inst.Direction * Inst.Steps) mod Circle_Size;
            if Start = 0 then
               Count := @ + 1;
            end if;
         end;
      end loop;
      return Count;
   end Part1;

   function Part2 (Instructions : Inst_Vectors.Vector) return Natural is
      Start : Natural := Start_Pos;
      Count : Natural := 0;
   begin
      for Inst of Instructions loop
         declare
            Nb_Tours        : constant Natural := Inst.Steps / Circle_Size;
            Steps_Restantes : constant Natural := Inst.Steps mod Circle_Size;
            New_Pos         : constant Integer :=
              (Start + Inst.Direction * Steps_Restantes);
         begin
            Count := @ + Nb_Tours;
            if (Inst.Direction = 1 and then New_Pos >= Circle_Size)
              or else (Inst.Direction = -1 and then New_Pos < 0)
            then
               Count := @ + 1;
            end if;

            -- Mettre a jour la position apres la rotation
            Start :=
              (Start + Steps_Restantes * Inst.Direction) mod Circle_Size;
         end;
      end loop;
      return Count;
   end Part2;

   Instructions : constant Inst_Vectors.Vector :=
     Read_Input ("inputs/day01/input.txt");

begin
   Put_Line ("Part 1 :" & Natural'Image (Part1 (Instructions)));
   Put_Line ("Part 2 :" & Natural'Image (Part2 (Instructions)));
exception
   when E : others =>
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (E));
end Day_01;
