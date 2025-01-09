with PWM_Motor;
with STM32.GPIO;
with STM32.Timers;
with STM32.Device;
with STM32.PWM;
with HAL;

package Steering_Motor is

   type Concrete_Steering_Motor is new PWM_Motor.Abstract_PWM_Motor with private;

   procedure Set_Angle (Self : in out Concrete_Steering_Motor; Angle : Float);
   procedure Set_Frequency (Self : in out Concrete_Steering_Motor; Frequency : Natural);
   procedure Set_Duty_Percent (Self : in out Concrete_Steering_Motor; Duty : Float);
   procedure Set_Duty_Microseconds (Self : in out Concrete_Steering_Motor; Microseconds : Natural);
   procedure Enable (Self : in out Concrete_Steering_Motor);
   procedure Disable (Self : in out Concrete_Steering_Motor);

private
   type Concrete_Steering_Motor is new PWM_Motor.Abstract_PWM_Motor with record
      Timer       : STM32.Timers.Timer_Access;
      PWM_Channel : STM32.PWM.PWM_Channel;
      Modulator   : STM32.PWM.PWM_Modulator;
      Angle       : Float := 0.0;
      Frequency   : Natural := 50;
      Duty_Cycle  : Float := 7.5;
      Enabled     : Boolean := False;
   end record;

end Steering_Motor;
