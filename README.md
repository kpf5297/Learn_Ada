

### **EPIC Template**
```markdown
# EPIC: [EPIC NAME]
> **Description:**  
[Brief description of the EPIC, its purpose, and scope.]

## Features
- [ ] [Feature: Feature Name](/issues/FEATURE_ISSUE_ID)  
- [ ] [Feature: Feature Name](/issues/FEATURE_ISSUE_ID)  
- [ ] [Feature: Feature Name](/issues/FEATURE_ISSUE_ID)  

## Linked Issues
- /relate FEATURE_ISSUE_ID_1  
- /relate FEATURE_ISSUE_ID_2  
- /relate FEATURE_ISSUE_ID_3  

---

### Example
# EPIC: Drive Control
> **Description:**  
This EPIC covers all tasks related to the development, testing, and refinement of the drive motor control system. It includes features like PWM control, rough speed control, and advanced functionalities like improved acceleration handling.

## Features
- [ ] [Feature: PWM Control](#1)  
- [ ] [Feature: Drive Motor](#2)  
- [ ] [Feature: Emergency Stop](#3)  
- [ ] [Feature: Improved Drive Speed Control](#4)  
- [ ] [Feature: Improved Drive Acceleration Control](#5)  

## Linked Issues
- /relate #1  
- /relate #2  
- /relate #3  
- /relate #4  
- /relate #5
```

---

### **Feature Template**
```markdown
# Feature: [FEATURE NAME]
> **Description:**  
[Brief description of the feature and its goals.]

## Tasks
- [ ] [Task: Task Name](/issues/TASK_ISSUE_ID)  
- [ ] [Task: Task Name](/issues/TASK_ISSUE_ID)  
- [ ] [Task: Task Name](/issues/TASK_ISSUE_ID)  

## Dependencies
- Depends on: /relate EPIC_ISSUE_ID  
- Related Issues:  
  - /relate TASK_ISSUE_ID_1  
  - /relate TASK_ISSUE_ID_2  

---

### Example
# Feature: PWM Control
> **Description:**  
This feature involves the implementation and testing of PWM control for both the drive and steering systems.

## Tasks
- [ ] [Task: Select pin mapping to be used for PWM control](#10)  
- [ ] [Task: Create basic Ada specification and body files to control PWM](#11)  
- [ ] [Task: Create abstract class utilizing PWM motor control](#12)  

## Dependencies
- Depends on: /relate #6 (Drive Control EPIC)  
- Related Issues:  
  - /relate #10  
  - /relate #11  
  - /relate #12
```

---

### **Task Template**
```markdown
# Task: [TASK NAME]
> **Description:**  
[Detailed description of the task, what needs to be done, and any relevant context.]

## Steps to Complete
- [ ] Step 1: [Specific task step]  
- [ ] Step 2: [Specific task step]  
- [ ] Step 3: [Specific task step]  

## Dependencies
- Depends on: /relate FEATURE_ISSUE_ID  
- Related Issues:  
  - /relate FEATURE_ISSUE_ID  
  - /relate OTHER_TASK_ISSUE_ID  

---

### Example
# Task: Select pin mapping to be used for PWM control
> **Description:**  
Research and decide on the pin mappings for PWM control for both the drive and steering motors.

## Steps to Complete
- [ ] Review STM32F429 datasheet for available PWM-capable pins.  
- [ ] Allocate pins for drive motor and steering motor PWM.  
- [ ] Document the pin mappings in a configuration file or README.  

## Dependencies
- Depends on: /relate #1 (PWM Control Feature)  
- Related Issues:  
  - /relate #6 (Drive Control EPIC)  
  - /relate #20 (Steering Control EPIC)
```

---

### How to Use These Templates

1. **Create an Issue for Each EPIC**:
   - Use the **EPIC Template**.
   - Replace placeholder text with your EPIC details and `/relate` the associated feature issues.

2. **Create an Issue for Each Feature**:
   - Use the **Feature Template**.
   - Add related tasks as checkboxes with links to their issues.
   - Link the feature to its parent EPIC using `/relate`.

3. **Create an Issue for Each Task**:
   - Use the **Task Template**.
   - Add detailed steps and `/relate` the associated feature and EPIC.

---

### **Branch Naming Convention**
1. **Structure:**  
   Use a hierarchical and descriptive format for branch names:
   ```
   <epic>/<feature>/<task-id>-<short-description>
   ```

   **Example:**  
   For EPIC "Drive Control," Feature "PWM Control," and Task ID `#42`:
   ```
   drive-control/pwm-control/42-setup-pwm-mapping
   ```

2. **Components of the Name:**
   - **`<epic>`**: The EPIC name in kebab-case (lowercase with dashes).
   - **`<feature>`**: The feature name in kebab-case.
   - **`<task-id>`**: The GitHub issue or task ID.
   - **`<short-description>`**: A concise description of the task in kebab-case.

---

### **Steps to Create a Branch for a Task**
1. **Identify the Task:**
   - Go to the GitHub issue corresponding to the task (e.g., Task `#42`).

2. **Create a New Branch:**
   Use the following command in your Git repository:
   ```
   git checkout -b <epic>/<feature>/<task-id>-<short-description>
   ```
   **Example:**
   ```
   git checkout -b drive-control/pwm-control/42-setup-pwm-mapping
   ```

3. **Link the Branch to the Task:**
   Use GitHub keywords in your commit messages or pull request descriptions to link the branch to the task:
   - **Keyword:** Use `Closes`, `Fixes`, or `Resolves` to automatically close the task issue when the branch is merged.
   - **Example Commit Message:**
     ```
     Fixes #42: Added basic PWM pin mapping logic
     ```

4. **Push the Branch to GitHub:**
   ```
   git push -u origin <epic>/<feature>/<task-id>-<short-description>
   ```

---

### **Best Practices for Branching**
- **Keep Branches Task-Specific:** Each branch should focus on a single task to keep the scope manageable.
- **Short-Lived Branches:** Merge branches into the main development branch (e.g., `main` or `develop`) as soon as the task is complete.
- **Use Pull Requests:** Always create a pull request (PR) to merge the branch. This allows for code review and ensures quality.

---

### **Example Workflow**
1. **EPIC:** `Drive Control`
2. **Feature:** `PWM Control`
3. **Task:** `#42 - Setup PWM Mapping`

#### Commands:
```bash
# Create a branch for the task
git checkout -b drive-control/pwm-control/42-setup-pwm-mapping

# Work on the task and commit changes
git commit -m "Fixes #42: Added PWM mapping for drive motor and steering motor"

# Push the branch to GitHub
git push -u origin drive-control/pwm-control/42-setup-pwm-mapping

# Create a Pull Request on GitHub to merge the branch
```

---

### **Benefits of This Approach**
- **Clarity:** The branch name clearly identifies its purpose and relation to EPICs, Features, and Tasks.
- **Traceability:** Links between branches and GitHub issues/tasks ensure easy tracking of progress.
- **Collaboration:** Team members can quickly understand the context of a branch and contribute effectively.
