from pydantic import BaseModel, EmailStr
from datetime import datetime
from typing import Optional

class AgentBase(BaseModel):
    first_name: str
    last_name: str
    email: EmailStr
    phone: str

class AgentCreate(AgentBase):
    pass

class Agent(AgentBase):
    id: int
    created_at: datetime
    updated_at: datetime

    class Config:
        from_attributes = True 