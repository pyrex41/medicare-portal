import re

with open("frontend/src/Onboarding/Onboarding.elm", "r") as f:
    content = f.read()

# Fix the missing closing parenthesis for Task.succeed ()
fixed_content = content.replace("(Task.perform (\\_ -> PlanSelection.LoadPlanFromSession plan) (Task.succeed ())", 
                                "(Task.perform (\\_ -> PlanSelection.LoadPlanFromSession plan) (Task.succeed ()))")
fixed_content = fixed_content.replace("(Task.perform (\\_ -> PlanSelection.LoadPlanFromSession planType) (Task.succeed ())", 
                                     "(Task.perform (\\_ -> PlanSelection.LoadPlanFromSession planType) (Task.succeed ()))")
fixed_content = fixed_content.replace("(Task.perform (\\_ -> UserDetails.loadUserFromSession userData) (Task.succeed ())", 
                                     "(Task.perform (\\_ -> UserDetails.loadUserFromSession userData) (Task.succeed ()))")

with open("frontend/src/Onboarding/Onboarding.elm", "w") as f:
    f.write(fixed_content)

print("File updated successfully")
