with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.String_Split; use GNAT.String_Split;

package body AOC_Utils is

   function Read_Lines (File_Name : String) return String_Vectors.Vector is
      File   : File_Type;
      Result : String_Vectors.Vector;
   begin
      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         Result.Append (To_Unbounded_String (Get_Line (File)));
      end loop;
      Close (File);
      return Result;
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end Read_Lines;

   function Read_Numbers (File_Name : String) return Natural_Vectors.Vector is
      File   : File_Type;
      Result : Natural_Vectors.Vector;
   begin
      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
         begin
            if Line'Length > 0 then
               Result.Append (Natural'Value (Line));
            end if;
         end;
      end loop;
      Close (File);
      return Result;
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end Read_Numbers;

   procedure Read_Two_Columns
     (File_Name : String;
      Left      : out Natural_Vectors.Vector;
      Right     : out Natural_Vectors.Vector)
   is
      File : File_Type;
   begin
      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         declare
            Line   : constant String := Get_Line (File);
            Fields : Slice_Set;
         begin
            Create (Fields, Line, " ", Multiple);
            if Slice_Count (Fields) >= 2 then
               Left.Append (Natural'Value (Slice (Fields, 1)));
               Right.Append (Natural'Value (Slice (Fields, 2)));
            end if;
         end;
      end loop;
      Close (File);
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end Read_Two_Columns;

   function Split
     (S : String; Separator : String := " ") return String_Vectors.Vector
   is
      Fields : Slice_Set;
      Result : String_Vectors.Vector;
   begin
      Create (Fields, S, Separator, Multiple);
      for I in 1 .. Slice_Count (Fields) loop
         Result.Append (To_Unbounded_String (Slice (Fields, I)));
      end loop;
      return Result;
   end Split;

   procedure Print_Result (Part : Positive; Result : String) is
   begin
      Put_Line ("Part" & Part'Image & ": " & Result);
   end Print_Result;

   function Abs_Diff (A, B : Integer) return Natural is
   begin
      return Natural (abs (A - B));
   end Abs_Diff;

end AOC_Utils;