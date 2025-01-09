package body Steering_Motor is

   procedure Set_Angle (Self : in out Concrete_Steering_Motor; Angle : Float) is
   begin
      Self.Angle := Angle;
      Self.Set_Duty_Percent(5.0 + (Angle / 180.0) * 5.0);
   end Set_Angle;

   procedure Set_Frequency (Self : in out Concrete_Steering_Motor; Frequency : Natural) is
   begin
      Self.Frequency := Frequency;
      STM32.PWM.Configure_PWM_Timer(Self.Timer, Frequency * 2);
   end Set_Frequency;

   procedure Set_Duty_Percent (Self : in out Concrete_Steering_Motor; Duty : Float) is
   begin
      Self.Duty_Cycle := Duty;
      Self.Modulator.Set_Duty_Time(Duty / 2);
   end Set_Duty_Percent;

   procedure Enable (Self : in out Concrete_Steering_Motor) is
   begin
      Self.Enabled := True;
      Self.Modulator.Enable_Output;
   end Enable;

   procedure Disable (Self : in out Concrete_Steering_Motor) is
   begin
      Self.Enabled := False;
   end Disable;

end Steering_Motor;
