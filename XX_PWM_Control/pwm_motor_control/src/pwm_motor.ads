with STM32.GPIO;    
with STM32.Timers;  
with STM32.PWM;     
with STM32.Device;
with HAL;

package PWM_Motor is

   type Abstract_PWM_Motor is abstract tagged private;

   procedure Set_Frequency (Self : in out Abstract_PWM_Motor; Frequency : Natural) is abstract;
   procedure Set_Duty_Percent (Self : in out Abstract_PWM_Motor; Duty : Float) is abstract;
   procedure Set_Duty_Microseconds (Self : in out Abstract_PWM_Motor; Microseconds : Natural) is abstract;
   procedure Enable (Self : in out Abstract_PWM_Motor) is abstract;
   procedure Disable (Self : in out Abstract_PWM_Motor) is abstract;

private
   type Abstract_PWM_Motor is abstract tagged null record;

end PWM_Motor;
