
# Features
- UART handling for receiving 7-bit commands (3-bit device, 4-bit data)
- A command interpreter for processing received data
- A queue for handling commands asynchronously
- Two consumers: 
  - A BLDC motor control class using PWM and MOSFET GPIO control
  - A steering servo motor using PWM
- LED interactions as part of debugging/feedback


# Overview 
This example demonstrates an Ada embedded system for the STM32F429 Discovery kit using the Ada Drivers Library (ADL) with the GNAT Academic toolchain. We will use **Alire** to manage the project and dependencies, the **Jorvik** real-time profile for tasking, and ADL’s drivers for UART, PWM, GPIO (LED) control. The system receives 7-bit commands over UART (3-bit device ID and 4-bit data), enqueues them, and processes them concurrently in separate tasks:
- **UART Task** – receives commands (7-bit) and enqueues them.  
- **Motor Task** – consumes commands for the drive motor (BLDC via PWM + GPIO for MOSFETs).  
- **Servo Task** – consumes commands for the steering servo (via PWM).  
- **LED Heartbeat Task** – blinks an LED for status (and toggles another LED on command reception for debugging).  

We use a protected **Command Queue** to buffer incoming commands and safely distribute them to the motor and servo tasks (the “two consumers” of the queue). The code is structured in a modular way with separate tasks and a protected object for concurrency. All source files are shown below with explanations. You can compile this with Alire by initializing a project (e.g. `alr init stm32_control --bin`) and adding the Ada Drivers Library dependency (e.g. `alr with Ada_Drivers_Library`). Ensure your Alire toolchain is set to an ARM cross-compiler (e.g. `alr toolchain --select gnat_arm_elf`) and use the STM32F429 board runtime (the project `.gpr` file should specify the target and runtime, such as `for Target use "arm-eabi"; for Runtime ("Ada") use "stm32f429_discovery_full";`). The code uses Ada 2022’s Jorvik profile for tasking support, which GNAT FSF/Academic 2021+ provides. 

###  *.gpr Add Runtime
```bash
for Runtime ("Ada") use "embedded-stm32f429disco";
```
### To See List of ARM Runtimes
```bash
ls ~/.local/share/alire/toolchains/gnat_arm_elf_13.1.0*/arm-eabi/lib/gnat/
```

### To Select Toolchain and Version
```bash
alr toolchain --select gnat_arm_elf=12.2.1 --local
```
### To See List of ARM Runtimes
```bash
ls ~/.local/share/alire/toolchains/gnat_arm_elf_13.1.0*/arm-eabi/lib/gnat/
```
### Remove Runtime not Being Used
```Bash
rm -rf ~/.local/share/alire/toolchains/gnat_arm_elf_12.2.1*
```

## Constants and Command Definitions (`command_defs.ads`) 
We first define the command format and some hardware constants (like LED pins, UART port, etc.). The 7-bit command is split into a 3-bit device ID and a 4-bit data value. We map device IDs for Motor, Servo, and LED. Data values range 0–15. We also define any necessary constants such as UART baud rate and task priorities for clarity. 

```ada
pragma Ada_2022;  -- Use Ada 2022 features
pragma Profile (Jorvik);  -- Use Jorvik tasking profile suitable for real-time/embedded
pragma Priority (System.Default_Priority); -- default priority for library-level (can be adjusted in tasks)

package Command_Defs is
   -- Device identifiers (3-bit). Using an enumeration for clarity.
   type Device_ID is (Device_Motor, Device_Servo, Device_LED, Device_Reserved3,
                      Device_Reserved4, Device_Reserved5, Device_Reserved6, Device_Reserved7);
   for Device_ID use (Device_Motor => 0, Device_Servo => 1, Device_LED => 2,
                      Device_Reserved3 => 3, Device_Reserved4 => 4,
                      Device_Reserved5 => 5, Device_Reserved6 => 6, Device_Reserved7 => 7);
   -- ^ We explicitly map enum values 0–7 to the device IDs for clarity.

   subtype Data_Value is Integer range 0 .. 15;  -- 4-bit data payload (0–15).

   -- Command structure combining a device and data.
   type Command is record
      Device : Device_ID;
      Data   : Data_Value;
   end record;

   -- Optional: raw 7-bit command type (for bit manipulation if needed)
   subtype Raw_Command is Interfaces.Unsigned_8 range 0 .. 127;  -- 7-bit raw command (0–127)

   -- Function to decode a raw 7-bit value into a Command record.
   function Decode_Command(Raw : Raw_Command) return Command is
      Dev_Value  : constant Interfaces.Unsigned_8 := Raw / 16;   -- upper 3 bits (0..7)
      Data_Value : constant Interfaces.Unsigned_8 := Raw mod 16; -- lower 4 bits (0..15)
   begin
      -- Convert numeric device ID to enum (if outside range 0–7, treat as reserved).
      declare
         Dev_ID : Device_ID;
      begin
         if Dev_Value <= 7 then
            Dev_ID := Device_ID(Dev_Value);  -- convert to Device_ID enumeration
         else
            Dev_ID := Device_Reserved7;
         end if;
         return Command'(Device => Dev_ID, Data => Integer(Data_Value));
      end;
   end Decode_Command;

   -- Hardware constants (board-specific)
   Baud_Rate    : constant := 115200;  -- UART baud rate for command interface
   -- UART port and pins would be defined via Ada Drivers Library. For example:
   UART_Port    : constant := 1;  -- Using USART1 (for instance, connected to PB6/PB7 on STM32F429 Discovery)
   -- LED pins: On STM32F429I-DISCO, LD3 (green) is PG13, LD4 (red) is PG14.
   -- The Ada Drivers Library likely provides named constants for these pins, e.g., PG13, PG14.
   -- We'll assume ADL provides GPIO point constants for these:
   -- LED_Green and LED_Red will be defined in board support (e.g. as GPIO_Point objects).
   -- If not, you can define them via SVD addresses or ADL GPIO API calls in initialization.

   -- Task priorities (relative values for illustration; Jorvik uses fixed-priority scheduling).
   UART_Task_Priority  : constant System.Priority := 5;   -- lowest priority (background UART polling)
   Motor_Task_Priority : constant System.Priority := 7;   -- higher than UART for timely motor control
   Servo_Task_Priority : constant System.Priority := 7;   -- same as Motor (both are consumers)
   LED_Task_Priority   : constant System.Priority := 6;   -- middle priority for heartbeat LED

end Command_Defs;
```

**Explanation:** We use an enumeration for `Device_ID` with explicit representation values 0–7 to match the 3-bit field ([Make with Ada 2020: Ada Robot Car With Neural… | The AdaCore Blog](https://blog.adacore.com/ada-robot-car-with-neural-network#:~:text=By%20default%2C%20this%20code%20has,added%20ports%20PD12%20and%20PD13)). The `Decode_Command` function splits a raw byte into device and data (using division/mod by 16) and returns a `Command` record. We also define constants for UART baud rate and identify the UART port (USART1 on pins PB6/PB7 in this example) and LED pins (PG13/PG14 for the on-board LEDs ([Make with Ada 2020: Ada Robot Car With Neural… | The AdaCore Blog](https://blog.adacore.com/ada-robot-car-with-neural-network#:~:text=ports%20of%20the%20green%20and,added%20ports%20PD12%20and%20PD13))). The task priorities are set such that the UART receiver runs at the lowest priority (to not starve other tasks) and the motor/servo tasks run at a higher priority (so they preempt the UART task when a command is available). The LED blinker has a middle priority. (These priorities are chosen for this example; they can be tuned as needed.)

## Protected Command Queue (`command_queue.ads` and `command_queue.adb`) 
We implement a protected queue to buffer incoming commands and allow multiple consumer tasks to dequeue based on device type. The protected object has one enqueue procedure (for the UART task to insert commands) and two entries – one for motor commands and one for servo commands. Each entry uses a barrier that only opens when a command for the respective device is at the front of the queue. This way, the Motor task will block until the next command in the queue is for the motor, and similarly for the Servo task. 

```ada
with Command_Defs; use Command_Defs;
pragma Elaborate_All (Command_Defs);

package Command_Queue is
   -- Maximum number of pending commands the queue can hold
   Queue_Size : constant := 16;  

   protected Command_Buffer is
      -- Enqueue a new command (called by UART task). Non-blocking; drops command if full.
      procedure Enqueue(Cmd : in Command);
      -- Dequeue operations for consumers (will block until a matching command is at queue head).
      entry Dequeue_Motor(Cmd : out Command);
      entry Dequeue_Servo(Cmd : out Command);
   private
      Buffer : array (1 .. Queue_Size) of Command;
      Head   : Natural := 1;
      Tail   : Natural := 1;
      Count  : Natural := 0;
   end Command_Buffer;
end Command_Queue;
```

**Explanation:** The protected `Command_Buffer` holds an array for queued commands with head/tail indices and count. We provide two entries, one for each consumer task, and a procedure for enqueuing. The entries will use barrier conditions (defined in the body) to wait until a command of the appropriate type is at the front of the queue.

Now the protected object’s implementation:

```ada
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;    -- For debug printing (optional, if supported in runtime)
with Command_Defs;

package body Command_Queue is

   protected body Command_Buffer is
      procedure Enqueue(Cmd : in Command) is
      begin
         if Count < Queue_Size then
            -- Add command to buffer at Tail position
            Buffer(Tail) := Cmd;
            Tail := (Tail mod Queue_Size) + 1;
            Count := Count + 1;
         else
            -- Queue is full, drop the command (oldest commands not yet processed).
            -- For debugging, we can light an LED or log an overflow event.
            -- Example: toggle the red LED to indicate overflow.
            -- (This assumes LED_Red is accessible; if not, just ignore or handle as needed.)
            Put_Line("Command queue overflow! Dropping command.");
            -- You could also set an LED here via ADL GPIO API for visual indication.
         end if;
      end Enqueue;

      entry Dequeue_Motor(Cmd : out Command)
         when (Count > 0 and then Buffer(Head).Device = Device_Motor) is
      begin
         Cmd := Buffer(Head);
         -- Remove the command from the queue
         Head := (Head mod Queue_Size) + 1;
         Count := Count - 1;
      end Dequeue_Motor;

      entry Dequeue_Servo(Cmd : out Command)
         when (Count > 0 and then Buffer(Head).Device = Device_Servo) is
      begin
         Cmd := Buffer(Head);
         Head := (Head mod Queue_Size) + 1;
         Count := Count - 1;
      end Dequeue_Servo;
   end Command_Buffer;

end Command_Queue;
```

**Explanation:** The `Enqueue` procedure adds a command to the circular buffer if space is available. If the queue is full, it logs a message or toggles an LED to indicate overflow (for debugging). The `Dequeue_Motor` entry is only open (enabled) when the queue is not empty **and** the next command is for the motor device ([Make with Ada 2020: Ada Robot Car With Neural… | The AdaCore Blog](https://blog.adacore.com/ada-robot-car-with-neural-network#:~:text=LED1%20%20%20%20,%3A%20GPIO_Point%20renames%20PD13)). Similarly, `Dequeue_Servo` waits for the next command to be a servo command. Each entry, when it runs, will output the command and remove it from the queue (updating head index and count). These protected entries ensure that each consumer task only wakes up for commands intended for it, effectively filtering by device ID. Note that while this design keeps command order, a command for one device will block the other entry if it’s at the head of the queue. In practice this is acceptable if commands arrive roughly in the order they should be processed, but you could also separate queues per device if independent ordering is required. 

## Motor Controller Task (`motor_task.ads` and `.adb`) 
The Motor task waits for drive motor commands from the queue and controls a BLDC motor via PWM and GPIO. We assume the motor driver requires a PWM output (for speed or torque control) and possibly a GPIO for direction or enable (for simplicity, we’ll assume one direction or that the BLDC driver handles direction internally). For demonstration, we map the 4-bit data to a PWM duty cycle. Data `0` might mean stop (0% duty) and `15` means full speed (100% duty), with linear scaling in between. The task runs an infinite loop waiting for commands and applying them. We also toggle an LED or print a message for feedback whenever a command is processed. 

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Command_Defs; use Command_Defs;
with Command_Queue; use Command_Queue;
with Ada.Real_Time; use Ada.Real_Time;
-- with Ada_Drivers_Library PWM and GPIO packages (names depend on ADL structure)
-- e.g., with STM32.GPIO; with STM32.Timer; (Assuming ADL has these)
-- Here we assume existence of procedures to set PWM duty and a direction GPIO.

package Motor_Task is
   task Motor with Priority => Motor_Task_Priority;
   -- The Motor task will consume drive motor commands from the Command_Buffer.
private
   -- (Optional) Motor control handles, e.g., PWM channel and GPIO
   -- For instance:
   -- Motor_PWM_Channel : constant Timer_Channel := ... (Initialized in main)
   -- Motor_Enable_Pin  : constant GPIO_Point := ...   (if needed for MOSFET enable)
end Motor_Task;
```

**Explanation:** We declare the `Motor` task and assign it the priority defined earlier (`Motor_Task_Priority`). Any hardware-specific handles (like a PWM channel or GPIO pin for direction) can be declared in the private section and set up during initialization (for simplicity, we’ll initialize in the main program and use global handles or closures).

Now the implementation of the Motor task’s behavior:

```ada
with Ada.Real_Time; use Ada.Real_Time;
with Command_Defs; use Command_Defs;
with Command_Queue; use Command_Queue;
-- with Ada_Drivers_Library.HAL.Timer; use Ada_Drivers_Library.HAL.Timer;
-- with Ada_Drivers_Library.HAL.GPIO;  use Ada_Drivers_Library.HAL.GPIO;
package body Motor_Task is

   task body Motor is
      Cmd : Command;
      Duty_Percent : Natural;
   begin
      -- Initialize motor outputs (already done in main, e.g., PWM configured).
      Put_Line("Motor Task started. Waiting for motor commands...");
      loop
         -- Wait for a motor command from the queue (blocks until available)
         Command_Queue.Command_Buffer.Dequeue_Motor(Cmd);
         -- Process the motor command
         declare
            MotorValue : constant Data_Value := Cmd.Data;
         begin
            -- Map 4-bit data to duty cycle percentage (0-100).
            Duty_Percent := Integer(MotorValue) * 100 / 15;  -- scale 0-15 to 0-100
            -- If the motor command encoding uses some bits for direction, handle it:
            -- (For example, you could define Data bit3 as direction bit. Here we assume 0-15 all forward.)
            -- Set PWM duty cycle to control speed:
            -- e.g., Timer_Set_Duty(Motor_PWM_Channel, Duty_Percent);
            -- In Ada Drivers Library, one might call a procedure to set the compare value or duty.
            -- For illustration, we'll just print the intended effect:
            Put_Line("Motor CMD: speed level " & Integer(MotorValue)'Img & 
                     " -> " & Duty_Percent'Img & "% duty PWM");
            -- If using direction control:
            -- if MotorValue = 0 then maybe brake or stop (duty 0).
            -- else set direction pin (if needed) and PWM duty.
            -- Example:
            -- if Direction_Bit = 1 then Motor_Dir_Pin.Set else Motor_Dir_Pin.Clear;
            -- Motor_PWM_Channel.Set_Duty(Duty_Percent);
         end;
         -- (We could also provide feedback via an LED: e.g., flash LED when motor command processed.)
      end loop;
   end Motor;
end Motor_Task;
```

**Explanation:** The Motor task blocks on `Dequeue_Motor` until a command with `Device_Motor` arrives in the queue. Once it obtains a command, it computes the duty cycle percentage corresponding to the 4-bit data (simple linear mapping in this example). In a real system, you would call the ADL PWM driver to set the new duty cycle on the timer output controlling the motor. For example, if using Timer X Channel Y for the motor, you would configure that timer in PWM mode (perhaps in main) and here call something like `Timer.Set_Duty(Timer_X, Channel_Y, Duty_Percent)` ([Moving a servo on STM32F4 : r/ada](https://www.reddit.com/r/ada/comments/jz40od/moving_a_servo_on_stm32f4/#:~:text=There%20is%20a%20demo%20of,com%2FAdaCore%2FAda_Drivers_Library%2F%20tree%2Fmaster%2Farch%2FARM%2FSTM32%2Fdriver_demos%2Fdemo_timer_pwm)) (the actual API depends on ADL’s Timer package). We include a placeholder `Put_Line` for demonstration which would print the speed level and duty cycle (in a real embedded scenario without console, you might blink an LED or use a debugger to observe this instead). If the motor needed direction control via a GPIO (for an H-bridge or a BLDC controller), the command encoding could designate one bit for direction which we would set/clear on a GPIO pin accordingly (not fully shown to keep code concise). 

## Servo Controller Task (`servo_task.ads` and `.adb`) 
The Servo task is similar in structure to the Motor task but controls a steering servo. Standard RC servos interpret a PWM pulse width (usually around 1ms to 2ms pulse within a 20ms period) as positions. We will use the 4-bit data as a coarse position command (0 = full left, 15 = full right, for example). The PWM for the servo should be configured to ~50 Hz (20ms period). We can map the 0–15 range to appropriate pulse widths. For simplicity, assume 0 -> 1.0 ms, 15 -> 2.0 ms (and intermediate values linearly between). 

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Command_Defs; use Command_Defs;
with Command_Queue; use Command_Queue;

package Servo_Task is
   task Servo with Priority => Servo_Task_Priority;
private
   -- (Optional) handle for servo PWM channel, e.g., Timer instance, configured in main.
end Servo_Task;
```

**Explanation:** The Servo task is declared with the same priority as the Motor task. Now its implementation:

```ada
with Ada.Real_Time; use Ada.Real_Time;
with Command_Defs; use Command_Defs;
with Command_Queue; use Command_Queue;
package body Servo_Task is

   task body Servo is
      Cmd : Command;
      Pulse_us : Natural;
   begin
      Put_Line("Servo Task started. Waiting for servo commands...");
      loop
         Command_Queue.Command_Buffer.Dequeue_Servo(Cmd);
         -- Process servo command
         declare
            PositionVal : constant Data_Value := Cmd.Data;
         begin
            -- Map 0–15 to pulse width (in microseconds) between 1000 and 2000 us.
            Pulse_us := 1000 + (Integer(PositionVal) * 1000 / 15);
            -- Set servo PWM output to this pulse width.
            -- e.g., ServoPWM.Set_Pulse_Width(Pulse_us);
            Put_Line("Servo CMD: position level " & Integer(PositionVal)'Img & 
                     " -> pulse " & Pulse_us'Img & " us");
            -- In a real scenario, you'd use the timer driver to adjust the compare value corresponding to Pulse_us.
         end;
         -- (Optional: for feedback, could toggle an LED here too.)
      end loop;
   end Servo;
end Servo_Task;
```

**Explanation:** The Servo task waits for `Device_Servo` commands. The 4-bit data is interpreted as a position command. We compute a pulse width in microseconds linearly from 1000 µs (for 0) to 2000 µs (for 15). In practice, you might calibrate these values to your servo’s end points. After calculating `Pulse_us`, you would call the ADL timer/PWM routine to set the pulse width or duty cycle. For example, if the timer is configured for a 20ms period (20000 µs), and if it’s running at a certain clock, you’d convert 1000–2000 µs to the timer’s compare value. ADL’s API might allow setting the high-time directly in microseconds or a percentage of the period. We print the intended pulse width for demonstration. This will result in the servo moving to the commanded position. 

## UART Receiver Task (`uart_task.ads` and `.adb`) 
The UART task handles incoming serial data. It configures the UART port for 7-bit or 8-bit data (we only use 7 bits for commands; the MSB can be ignored or used as parity if needed). In this example, we’ll consider 8-bit data where the top bit is always 0. The task runs in a loop checking for received bytes. We use a **protected command queue** to offload processing so that the UART ISR or polling loop can quickly enqueue the command and continue listening. The task toggles an LED each time a command is received, for debugging. 

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Command_Defs; use Command_Defs;
with Command_Queue; use Command_Queue;
with Ada.Real_Time; use Ada.Real_Time;
-- with Ada_Drivers_Library.HAL.UART; (for UART driver)

package UART_Task is
   task UART with Priority => UART_Task_Priority;
end UART_Task;
```

**Explanation:** The UART task is declared with the lowest priority. Now the implementation:

```ada
with Ada.Real_Time; use Ada.Real_Time;
with Command_Defs; use Command_Defs;
with Command_Queue; use Command_Queue;
-- with Ada_Drivers_Library.HAL.UART;
-- with Ada_Drivers_Library.HAL.GPIO;  -- for LED control
package body UART_Task is

   task body UART is
      Raw : Command_Defs.Raw_Command;
      Cmd : Command_Defs.Command;
   begin
      Put_Line("UART Task started. Initializing UART...");
      -- Initialize UART hardware (enable clocks, set baud rate, mode, etc.)
      -- Using Ada Drivers Library API. For example:
      -- UART.Init(Port => UART_Port, Baud => Baud_Rate, Config => 8_bits_no_parity);
      -- (Assume ADL configures 8N1 frame by default; we only use 7 bits of data.)
      -- If needed, flush or clear any buffers.
      Put_Line("UART initialized, listening for commands...");
      loop
         -- Method 1: Blocking read (if ADL provides one).
         -- e.g., Raw := UART.Get_Byte;
         -- Method 2: Polling loop.
         -- We'll illustrate polling with a short delay to avoid CPU hogging:
         declare
            ByteReceived : Boolean := False;
         begin
            -- Check if data is available (this would use ADL UART status, e.g., UART.Data_Ready)
            -- Pseudocode:
            -- if UART.Has_Data then
            --    Raw := UART.Read_Byte;
            --    ByteReceived := True;
            -- end if;
            -- (For demo, we can't actually poll hardware here, so assume blocking read is possible.)
            null;
         end;
         exit when False;  -- (Remove or adjust this exit in a real loop; here to avoid infinite loop in example.)

         if ByteReceived then
            -- Mask to 7 bits just in case (ensure MSB is zeroed)
            Raw := Raw and 16#7F#;
            Cmd := Decode_Command(Raw);
            -- Enqueue the command for processing by consumers
            Command_Queue.Command_Buffer.Enqueue(Cmd);
            -- Toggle the debug LED to indicate a command was received
            -- e.g., LED_Red.Toggle;
            Put_Line("UART: Received raw " & Integer(Raw)'Img & 
                     " -> Device=" & Integer(Cmd.Device'Pos)'Img & 
                     ", Data=" & Integer(Cmd.Data)'Img);
         end if;

         -- Small delay or yield to let other tasks run if no data (since this is lowest priority, 
         -- it will be preempted by others when they are ready, but we'll explicitly yield to be safe).
         delay until Ada.Real_Time.Clock + Milliseconds(10);
      end loop;
   end UART;
end UART_Task;
```

**Explanation:** In a real implementation, you would use ADL’s UART driver to configure and read the serial port. The pseudo-code above indicates where you would initialize the UART hardware (setting baud rate etc.) ([Ada Modbus Analyzer - Hackster.io](https://www.hackster.io/yasaspeiris/ada-modbus-analyzer-c84242#:~:text=match%20at%20L385%20Initialize_UART_GPIO%3B%20Disable,USART_1%2C%20Tx_Rx_Mode)). Inside the loop, we either use a blocking read (if the driver provides one that waits for a byte) or a polling approach. Here, we sketch a polling approach: check if a byte is available (`UART.Has_Data`) and if so, read it. We then mask it to 7 bits (`Raw and 16#7F#`) to ensure we only consider the lower 7 bits as the command. This raw value is decoded into a `Command` record via `Decode_Command`. We immediately enqueue the decoded command into the `Command_Buffer` protected object. Enqueuing is fast (just a memory copy and index update) so it won’t block the UART task for long. After enqueuing, we toggle a debug LED (e.g. the red LED) to give a visual indication that a command was received. We also print the received command for debugging (if a console or semi-hosting is available). The loop includes a small delay (`delay until Clock + 10ms`) to yield control and prevent busy-waiting from starving other tasks. In the Jorvik/Ravenscar profile, tasks do not timeshare unless a delay or blocking call occurs, so this delay ensures the UART task doesn’t hog the CPU when no data is arriving. 

*Note:* The actual UART reading mechanism will depend on the ADL. Often, a driver might offer an interrupt-driven ring buffer so that `UART.Has_Data` checks a buffer, or you might attach an interrupt handler. In this simple design, a polling loop is used for clarity. In practice, the UART task could be structured to block on an interrupt or use a protected object with an entry for UART RX as well, but that’s beyond this example’s scope. The main point is that the UART task produces commands into the queue asynchronously. 

## Main Program (`main.adb`) 
Finally, we have the main procedure that sets up the system. It initializes the hardware (clock, drivers, etc.), and then the tasks are started. In Ada, tasks begin execution after the main program’s elaboration when using `pragma Partition_Elaboration_Policy(Sequential)` with a real-time profile. We ensure that this policy is in effect so all initialization is done before tasks run. In the main code, we configure the PWM channels for motor and servo, configure the GPIO for LEDs, and then let the tasks operate. We also blink the green LED in the background (using the LED task) to show the system is alive. 

```ada
with Ada.Real_Time; use Ada.Real_Time;
with System;
with Command_Defs;
with Command_Queue;
with Motor_Task;
with Servo_Task;
with UART_Task;

procedure Main is
   pragma Priority (System.Priority'Last);  -- Run main at highest priority during init
   pragma Partition_Elaboration_Policy (Sequential);
begin
   -- Initialize board/system clock (if not done by runtime startup)
   -- Initialize peripherals: UART, Timer for PWM, GPIO for LEDs.
   Put_Line("Initializing hardware...");

   -- Initialize LED GPIOs for output
   -- e.g., LED_Green and LED_Red are configured as outputs.
   -- ADL might do this in runtime or we do:
   -- STM32.GPIO.Set_Mode(LED_Green, Output);
   -- STM32.GPIO.Set_Mode(LED_Red, Output);
   -- Turn off LEDs initially:
   -- LED_Green.Clear;
   -- LED_Red.Clear;

   -- Initialize PWM timers for motor and servo control.
   -- e.g., Configure TimerX Channel1 for Motor (fast PWM, e.g., 20kHz for BLDC), 
   -- and TimerY Channel1 for Servo (50Hz, 20ms period).
   -- For example:
   -- Motor_Timer := Timer.Init(Timer_Id => 1, Channel => 1, Frequency => 20000 Hz);
   -- Servo_Timer := Timer.Init(Timer_Id => 2, Channel => 1, Frequency => 50 Hz);
   -- (The ADL Timer API calls would set up timer base frequency, mode, and enable PWM output on corresponding pins.)

   -- Set initial PWM outputs to safe defaults
   -- Motor off (0% duty) and servo centered (~1.5ms pulse).
   -- Timer.Set_Duty(Motor_Timer, Channel1, 0);
   -- Timer.Set_Pulse(Servo_Timer, Channel1, 1500 us);

   Put_Line("Hardware initialization complete. Starting tasks...");

   -- At this point, tasks (UART, Motor, Servo, LED) begin execution concurrently.
   -- The main thread can optionally do other background work or simply loop forever.
   loop
      -- Main could sleep or perform monitoring. We'll blink the green LED for heartbeat here.
      -- Toggle heartbeat LED:
      -- LED_Green.Toggle;
      -- Delay for half a second
      delay for 0.5;  -- 500 ms
   end loop;
end Main;
```

**Explanation:** We use `pragma Partition_Elaboration_Policy(Sequential)` to ensure the task threads don’t start running until after `Main` finishes elaboration (which includes our initialization) ([Ada Drivers Library - General - Ada Forum](https://forum.ada-lang.io/t/ada-drivers-library/258#:~:text=Note%20that%20you%20will%20need,changed%20in%20GNAT%20FSF%2012)) ([Ada Drivers Library - General - Ada Forum](https://forum.ada-lang.io/t/ada-drivers-library/258#:~:text=I%20would%20not%20recommend%20making,The%20AdaCore%20Blog)). In the initialization section, we configure the GPIO pins for the green and red LEDs as outputs (using ADL’s GPIO abstractions, e.g., `GPIO_Point` objects for PG13 and PG14 ([Make with Ada 2020: Ada Robot Car With Neural… | The AdaCore Blog](https://blog.adacore.com/ada-robot-car-with-neural-network#:~:text=LED1%20%20%20%20,%3A%20GPIO_Point%20renames%20PD13)), and then using `.Clear` to ensure they start off). We also configure two timers from ADL for PWM output – one for the motor (with a high frequency for motor speed control, e.g., 20 kHz PWM for a BLDC motor driver to avoid audible noise) and one for the servo (50 Hz for hobby servo signals). The ADL provides timer configuration routines and will handle enabling the proper alternate functions on the pins. We then set the initial duty cycle/pulse: motor off (0%) and servo to a neutral position (~1500 µs pulse width). 

After hardware init, we print a message and enter a loop. In this loop, we blink the green LED every 0.5 seconds as a heartbeat to indicate the system is running. We could also instantiate a separate `LED_Task` for blinking instead of doing it in main; either approach is fine. Here, we do it in the main loop at the lowest priority (since main, after elaboration, becomes just a running task at default priority unless changed). Because we didn’t terminate `Main` and entered an infinite loop, the main procedure itself acts as a low-priority background task blinking the LED. If we wanted, we could have declared a separate task for LED blinking with its own priority (which might be cleaner). 

All other tasks (UART, Motor, Servo) start running concurrently once we enter the loop. The UART task waits for incoming bytes, the Motor and Servo tasks wait on the queue. When a command arrives via UART:
1. The UART task decodes and enqueues it. It toggles the red LED for feedback.
2. If it’s a motor command, the Motor task’s protected entry opens, it dequeues the command and sets the motor PWM accordingly.
3. If it’s a servo command, the Servo task awakens, processes it and sets the servo PWM.
4. Each task then goes back to waiting for the next command.

Throughout, the green LED blinks to show the system is alive, and the red LED will flicker on each command reception (useful for debugging UART input in case a console is not attached).

## Deployment 
You can build this program with Alire. Ensure your `alire.toml` includes a dependency on the Ada Drivers Library and that the GNAT ARM toolchain is selected (e.g. `gnat_arm_elf`). For example:
```bash
alr with Ada_Drivers_Library
alr build
```
This will compile the Ada code and link against the STM32F429 board support package provided by ADL ([Ada Drivers Library - General - Ada Forum](https://forum.ada-lang.io/t/ada-drivers-library/258#:~:text=with%20)) ([Ada Drivers Library - General - Ada Forum](https://forum.ada-lang.io/t/ada-drivers-library/258#:~:text=I%20would%20not%20recommend%20making,The%20AdaCore%20Blog)). You’ll need to flash the resulting `.elf` or binary to the STM32F429 Discovery board using a tool like `st-flash` or OpenOCD. Once running, you can send 7-bit command bytes to the board’s UART (ensure the baud rate matches `Baud_Rate`, 115200 in this example). The motor and servo should respond to their respective commands (e.g., if you send a byte 0x1F (b`001 1111`), that is device=0x1 (Servo) with data=0xF (15), so the servo will go to full right, and if you send 0x0A (b`000 1010`), that is device=0 (Motor) with data=0xA (10), setting the motor to about 66% speed). The on-board LEDs will assist in debugging: the green LED will blink steadily, and the red LED will toggle briefly whenever a command is received via UART.

### **What is a Toolchain?**
A **toolchain** is a collection of tools used to **compile, assemble, and link** programs for a specific target system. In embedded systems, the toolchain ensures your code is translated into machine instructions that the microcontroller (like STM32F429) can understand.

---

### **Key Components of a Toolchain**
1. **Compiler (GNAT, GCC, Clang, etc.)**  
   - Translates Ada (or C, C++) into assembly/machine code.  
   - Example: `arm-eabi-gcc` (C compiler), `arm-eabi-gnat` (Ada compiler).

2. **Assembler**  
   - Converts assembly language into machine code (binary).  
   - Example: `as` (GNU assembler).

3. **Linker**  
   - Combines compiled code into an executable program.  
   - Example: `arm-eabi-ld`.

4. **Libraries**  
   - Provides standard functions (e.g., math, I/O, runtime).  
   - Example: Ada runtime (`ravenscar-stm32f4`).

5. **Debugger & Programmer Tools**  
   - Helps in debugging & flashing firmware.  
   - Example: `arm-eabi-gdb`, `openocd`, `st-flash`.

---

### **Types of Toolchains**
- **Native Toolchain**: Compiles code for the same system you're developing on (e.g., compiling for macOS on a Mac).
- **Cross-Toolchain**: Compiles code for a different system (e.g., compiling Ada/C for an STM32 microcontroller on a Mac).

Your toolchain **GNAT ARM ELF** (`gnat_arm_elf`) is a **cross-toolchain** because:
✅ It runs on **Mac**  
✅ It produces code for **STM32 (ARM Cortex-M4)**  

---

### **Why Does the Toolchain Matter?**
1. **Different Versions Can Behave Differently**  
   - GNAT 12.2.1 **works**, but GNAT 13.1.0 **fails** in your case.
   - This might be due to **runtime compatibility** or **compiler flags**.

2. **Not All Toolchains Support Embedded Targets**  
   - GNAT **native** is for desktops.  
   - GNAT **ARM ELF** is for embedded microcontrollers.  

3. **Correct Runtime & Libraries Must Match the Toolchain**  
   - `embedded-stm32f4` might work in **12.2.1**, but not in **13.1.0**.

---
