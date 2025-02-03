from pydantic import BaseModel, EmailStr, Field
from datetime import date
from typing import Optional

class ContactBase(BaseModel):
    first_name: str
    last_name: str
    email: EmailStr
    current_carrier: str
    plan_type: str
    effective_date: date
    birth_date: date
    tobacco_user: bool = False
    gender: str = Field(..., pattern="^[MF]$")
    state: str
    zip_code: str
    agent_id: Optional[int] = None
    last_emailed_date: Optional[date] = None

class ContactCreate(ContactBase):
    pass

class Contact(ContactBase):
    id: int
    created_at: date
    updated_at: date

    class Config:
        from_attributes = True 