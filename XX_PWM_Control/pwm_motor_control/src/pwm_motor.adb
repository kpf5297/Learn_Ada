package body PWM_Motor is

   procedure Set_Frequency (Self : in out Abstract_PWM_Motor; Frequency : Natural) is abstract;
   procedure Set_Duty_Percent (Self : in out Abstract_PWM_Motor; Duty : Float) is abstract;
   procedure Set_Duty_Microseconds (Self : in out Abstract_PWM_Motor; Microseconds : Natural) is abstract;
   procedure Enable (Self : in out Abstract_PWM_Motor) is abstract;
   procedure Disable (Self : in out Abstract_PWM_Motor) is abstract;

end PWM_Motor;
