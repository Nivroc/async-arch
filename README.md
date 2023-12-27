# 1. Requirements Analysis
## Task tracker

- The task tracker should be a separate dashboard and accessible to all employees of UberPopug Inc. The task tracker should contain only tasks. There are no projects, scopes, or sprints because they do not fit in a popug's head. Each employee should be able to see a list of tasks assigned to them in a separate place.
  - Actor - User
  - Command - Access tasks
  - Data - Task Information (id, desc, status, assignee, cost, progress ect.)
  - Event - User.TasksFetched
- Authorization in the task tracker must be performed through the common authorization service of UberPopug Inc (we have an innovative authorization system based on the shape of the beak).
    - Actor - User
    - Command - LoginToTT
    - Data - User Info (uid, beak shape, open Tasks)
    - Event - User.LoggedIn
- Anyone (administrator, boss, developer, manager, and any other role) can create new tasks. A task should have a description, a status (completed or not), and a randomly selected popug (except for managers and administrators) assigned to the task. The price for the task is determined once, at the moment it appears in the system (can be with a minimal delay). Money is debited immediately after the task is assigned to an employee.
    - Actor - User
    - Command - CreateTask
    - Data - Task Info (id, desc, status, assignee, cost, reward, progress ect.)
    - Event - User.TaskCreated
- Managers or administrators should have a "assign tasks" button, which will take all open tasks and randomly assign each to any of the employees (except for managers and administrators). If you didn't manage to complete the task before reassignment - sorry, move on to the next one.
    - Actor - Manager | Admin
    - Command - Shuffle
    - Data - Task Info, User Info
    - Event - Actor.TasksShuffled
- Every employee should have the ability to mark a task as completed. Money is credited after the completion of the task.
    - Actor - User
    - Command - Complete Task
    - Data - Task Info, User Info
    - Event - User.TaskClosed
## Accounting
 - Regular popugs should only have access to information about their own accounts (audit log + current balance).
    - Actor - Worker
    - Command - FetchAuditLog
    - Data - Audit log, Balance
    - Event - User.AuditLogFetched
 - Administrators and accountants should have access to overall financial statistics, including the amount of money earned (total amount earned by top management today + daily statistics).
    - Actor - Admin | Accountant
    - Command - FetchStats
    - Data - Task Info, User Info
    - Event - Actor.StatsFetched
- Authorization in the task tracker must be performed through the common authorization service of UberPopug Inc (we have an innovative authorization system based on the shape of the beak).
    - Actor - User
    - Command - LoginToAc
    - Data - User Info (uid, beak shape, networth, open Tasks)
    - Event - User.LoggedIn
- To be able to send amount via the email
    - Actor - Accounting
    - Command - SendAllBalances
    - Data - Revenue
    - Event - Accounting.EmailsSent
- After the balance payout (at the end of the day), it should be reset, and the accounting audit log should display that the amount has been paid out.
    - Actor - Accounting
    - Command - AwardMoney
    - Data - User Info(uid, beak shape, balance)
    - Event - Accounting.FinancesAwarded

## Analytics

- Analytics is a separate dashboard, available only to administrators.
    - Actor - Admin
    - Command - LoginToAn
    - Data - User Info (uid, beak shape)
    - Event - Admin.LoggedIn
- It is necessary to display the most expensive task of the day, week, or month.
    - Actor - Admin
    - Command - FetchAnData
    - Data - Cost History
    - Event - Admin.AnDataFetched


# Miro

https://miro.com/welcomeonboard/RjVJNVFyU3I2SEtLMEhpWHd6U2ZUY1BMZU1IVzZzZ1NZdUc1c3FBSm5HVE1tU21qNkNpNFNiNVdJQm9PWXAyaXwzMDc0NDU3MzQ2MDQ2NjYyOTUxfDI=?share_link_id=733413422784

![image info](./Miro.png)
