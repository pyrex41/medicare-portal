from fastapi import FastAPI, HTTPException, Depends
from db.database import get_db, Database
from models.contact import ContactCreate
from models.agent import AgentCreate
from pydantic import EmailStr
import logging
from fastapi.middleware.cors import CORSMiddleware
import json
import os
from typing import Optional
from fastapi.responses import JSONResponse

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

app = FastAPI()

# Add CORS middleware to allow frontend requests
app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:5173"],  # Add your frontend URL
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Load ZIP code data
try:
    with open('zipData.json', 'r') as f:
        ZIP_DATA = json.load(f)
except Exception as e:
    logger.error(f"Error loading ZIP data: {e}", exc_info=True)
    ZIP_DATA = {}

@app.get("/api/contacts")
def get_contacts(db: Database = Depends(get_db)):
    try:
        logger.info("Attempting to fetch contacts")
        contacts = db.fetch_all(
            "SELECT * FROM contacts ORDER BY created_at DESC LIMIT 100"
        )
        logger.info(f"Successfully fetched {len(contacts)} contacts")
        
            
        # Convert rows to dictionaries and log each conversion
        result = []
        for contact in contacts:
            contact_dict = {
                'id': contact[0],
                'first_name': contact[1],
                'last_name': contact[2],
                'email': contact[3],
                'current_carrier': contact[4],
                'plan_type': contact[5],
                'effective_date': contact[6],
                'birth_date': contact[7],
                'tobacco_user': bool(contact[8]),  # Convert integer to boolean
                'gender': contact[9],
                'state': contact[10],
                'zip_code': contact[11],
                'last_emailed': contact[12],
                'created_at': contact[13],
                'updated_at': contact[14]
            }
            result.append(contact_dict)
        
        return result
        
    except Exception as e:
        logger.error(f"Error in get_contacts: {e}", exc_info=True)
        raise HTTPException(status_code=500, detail=str(e))

@app.post("/api/contacts")
def create_contact(contact: ContactCreate, db: Database = Depends(get_db)):
    try:
        logger.info(f"Attempting to create contact: {contact.first_name} {contact.last_name}")
        query = """
            INSERT INTO contacts (
                first_name, last_name, email, current_carrier, plan_type,
                effective_date, birth_date, tobacco_user, gender,
                state, zip_code, agent_id
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            RETURNING *
        """
        params = (
            contact.first_name,
            contact.last_name,
            contact.email,
            contact.current_carrier,
            contact.plan_type,
            contact.effective_date.isoformat(),
            contact.birth_date.isoformat(),
            1 if contact.tobacco_user else 0,
            contact.gender,
            contact.state,
            contact.zip_code,
            contact.agent_id
        )
        
        logger.info("Executing insert query")
        result = db.execute(query, params)
        row = result.fetchone()
        logger.info("Insert successful")
        
        return {
            'id': row[0],
            'first_name': row[1],
            'last_name': row[2],
            'email': row[3],
            'current_carrier': row[4],
            'plan_type': row[5],
            'effective_date': row[6],
            'birth_date': row[7],
            'tobacco_user': bool(row[8]),
            'gender': row[9],
            'state': row[10],
            'zip_code': row[11],
            'agent_id': row[12],
            'last_emailed_date': row[13],
            'created_at': row[14],
            'updated_at': row[15]
        }
    except Exception as e:
        logger.error(f"Error creating contact: {e}", exc_info=True)
        raise HTTPException(status_code=500, detail=str(e))

@app.put("/api/contacts/{contact_id}")
def update_contact(contact_id: int, contact: ContactCreate, db: Database = Depends(get_db)):
    try:
        logger.info(f"Updating contact with ID: {contact_id}")
        
        # If agent_id is None, we'll exclude it from the update
        if contact.agent_id is None:
            query = """
                UPDATE contacts SET 
                    first_name = ?,
                    last_name = ?,
                    email = ?,
                    current_carrier = ?,
                    plan_type = ?,
                    effective_date = ?,
                    birth_date = ?,
                    tobacco_user = ?,
                    gender = ?,
                    state = ?,
                    zip_code = ?,
                    updated_at = CURRENT_TIMESTAMP
                WHERE id = ?
                RETURNING *
            """
            params = (
                str(contact.first_name),
                str(contact.last_name),
                str(contact.email),
                str(contact.current_carrier),
                str(contact.plan_type),
                contact.effective_date.isoformat(),
                contact.birth_date.isoformat(),
                1 if contact.tobacco_user else 0,
                str(contact.gender),
                str(contact.state),
                str(contact.zip_code),
                contact_id
            )
        else:
            query = """
                UPDATE contacts SET 
                    first_name = ?,
                    last_name = ?,
                    email = ?,
                    current_carrier = ?,
                    plan_type = ?,
                    effective_date = ?,
                    birth_date = ?,
                    tobacco_user = ?,
                    gender = ?,
                    state = ?,
                    zip_code = ?,
                    agent_id = ?,
                    updated_at = CURRENT_TIMESTAMP
                WHERE id = ?
                RETURNING *
            """
            params = (
                str(contact.first_name),
                str(contact.last_name),
                str(contact.email),
                str(contact.current_carrier),
                str(contact.plan_type),
                contact.effective_date.isoformat(),
                contact.birth_date.isoformat(),
                1 if contact.tobacco_user else 0,
                str(contact.gender),
                str(contact.state),
                str(contact.zip_code),
                contact.agent_id,
                contact_id
            )
        
        logger.info(f"Query params: {params}")
        result = db.execute(query, params)
        row = result.fetchone()
        if not row:
            raise HTTPException(status_code=404, detail="Contact not found")
            
        return {
            'id': row[0],
            'first_name': row[1],
            'last_name': row[2],
            'email': row[3],
            'current_carrier': row[4],
            'plan_type': row[5],
            'effective_date': row[6],
            'birth_date': row[7],
            'tobacco_user': bool(row[8]),
            'gender': row[9],
            'state': row[10],
            'zip_code': row[11],
            'agent_id': row[12],
            'last_emailed_date': row[13],
            'created_at': row[14],
            'updated_at': row[15]
        }
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error updating contact: {e}", exc_info=True)
        raise HTTPException(status_code=500, detail=str(e))

@app.get("/api/zip-lookup/{zip_code}")
def lookup_zip(zip_code: str):
    try:
        # Look up the ZIP code in our data
        zip_info = ZIP_DATA.get(zip_code)
        
        if zip_info is None:
            raise HTTPException(status_code=404, detail="ZIP code not found")
            
        return {
            "state": zip_info["state"],
            "counties": zip_info["counties"],
            "cities": zip_info["cities"]
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error looking up ZIP code: {e}", exc_info=True)
        raise HTTPException(status_code=500, detail=str(e))

# Add new endpoints for agents
@app.post("/api/agents")
def create_agent(agent: AgentCreate, db: Database = Depends(get_db)):
    try:
        query = """
            INSERT INTO agents (first_name, last_name, email, phone)
            VALUES (?, ?, ?, ?)
            RETURNING *
        """
        params = (agent.first_name, agent.last_name, agent.email, agent.phone)
        
        result = db.execute(query, params)
        row = result.fetchone()
        
        return {
            'id': row[0],
            'first_name': row[1],
            'last_name': row[2],
            'email': row[3],
            'phone': row[4],
            'created_at': row[5],
            'updated_at': row[6]
        }
    except Exception as e:
        logger.error(f"Error creating agent: {e}", exc_info=True)
        raise HTTPException(status_code=500, detail=str(e))

@app.get("/api/agents")
def get_agents(db: Database = Depends(get_db)):
    try:
        agents = db.fetch_all("SELECT * FROM agents ORDER BY created_at DESC")
        return [
            {
                'id': row[0],
                'first_name': row[1],
                'last_name': row[2],
                'email': row[3],
                'phone': row[4],
                'created_at': row[5],
                'updated_at': row[6]
            }
            for row in agents
        ]
    except Exception as e:
        logger.error(f"Error fetching agents: {e}", exc_info=True)
        raise HTTPException(status_code=500, detail=str(e))

@app.on_event("startup")
async def startup_event():
    logger.info("Application starting up")
    # Test database connection
    db = get_db()
    try:
        conn = db.get_connection()
        result = conn.execute("SELECT 1").fetchone()
        logger.info("Database connection test successful")
    except Exception as e:
        logger.error(f"Database connection test failed: {e}", exc_info=True)
        raise 