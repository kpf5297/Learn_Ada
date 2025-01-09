with PWM_Motor;
with STM32.GPIO;
with STM32.Timers;
with STM32.Device;
with STM32.PWM;
with HAL;

package Drive_Motor is

   type Concrete_Drive_Motor is new PWM_Motor.Abstract_PWM_Motor with private;

   procedure Set_Speed (Self : in out Concrete_Drive_Motor; Speed : Float);
   procedure Set_Direction (Self : in out Concrete_Drive_Motor; Forward : Boolean);
   procedure Set_Frequency (Self : in out Concrete_Drive_Motor; Frequency : Natural);
   procedure Set_Duty_Percent (Self : in out Concrete_Drive_Motor; Duty : Float);
   procedure Set_Duty_Microseconds (Self : in out Concrete_Drive_Motor; Microseconds : Natural);
   procedure Enable (Self : in out Concrete_Drive_Motor);
   procedure Disable (Self : in out Concrete_Drive_Motor);

private
   type Concrete_Drive_Motor is new PWM_Motor.Abstract_PWM_Motor with record
      Timer       : STM32.Timers.Timer;
      Modulator   : STM32.PWM.PWM_Modulator;
      Speed       : Float := 0.0;
      Forward     : Boolean := True;
      Frequency   : Natural := 1000;
      Duty_Cycle  : Float := 50.0;
      Enabled     : Boolean := False;
   end record;

end Drive_Motor;
