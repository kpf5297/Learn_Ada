package body Drive_Motor is

   procedure Set_Speed (Self : in out Concrete_Drive_Motor; Speed : Float) is
   begin
      Self.Speed := Speed;
      Self.Set_Duty_Percent(Speed);
   end Set_Speed;

   procedure Set_Direction (Self : in out Concrete_Drive_Motor; Forward : Boolean) is
   begin
      Self.Forward := Forward;
   end Set_Direction;

   procedure Set_Frequency (Self : in out Concrete_Drive_Motor; Frequency : Natural) is
   begin
      Self.Frequency := Frequency;
      STM32.PWM.Configure_PWM_Timer(Self.Timer'Access, Frequency * 2);
   end Set_Frequency;

   procedure Set_Duty_Percent (Self : in out Concrete_Drive_Motor; Duty : Float) is
   begin
      Self.Duty_Cycle := Duty;
      Self.Modulator.Set_Duty_Time(Duty / 2);
   end Set_Duty_Percent;

   procedure Enable (Self : in out Concrete_Drive_Motor) is
   begin
      Self.Enabled := True;
      Self.Modulator.Enable_Output;
   end Enable;

   procedure Disable (Self : in out Concrete_Drive_Motor) is
   begin
      Self.Enabled := False;
   end Disable;

end Drive_Motor;
