from fastapi import FastAPI, HTTPException, Depends, UploadFile, File, Form
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
import csv
import io
from datetime import datetime
from pprint import pprint
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
        result = db.connection.execute("SELECT 1").fetchone()
        logger.info("Database connection test successful")
    except Exception as e:
        logger.error(f"Database connection test failed: {e}", exc_info=True)
        raise

@app.post("/api/contacts/upload")
async def upload_contacts(
    file: UploadFile = File(...), 
    overwrite_duplicates: bool = Form(False),
    db: Database = Depends(get_db)
):
    try:
        contents = await file.read()
        decoded = contents.decode()
        csv_reader = csv.DictReader(io.StringIO(decoded))
        
        # Required fields in desired order
        required_fields_ordered = [
            'First Name',
            'Last Name',
            'Email',
            'Current Carrier',
            'Plan Type',
            'Effective Date',
            'Birth Date',
            'Tobacco User',
            'Gender',
            'ZIP Code'
        ]
        required_fields = set(required_fields_ordered)
        
        # Validate headers
        headers = set(csv_reader.fieldnames)
        if not required_fields.issubset(headers):
            missing_fields = required_fields - headers
            return JSONResponse(
                status_code=200,
                content={
                    "success": False,
                    "message": f"Missing required columns: {', '.join(missing_fields)}",
                    "error_csv": None,
                    "total_rows": 0,
                    "error_rows": 0,
                    "valid_rows": 0
                }
            )
        
        valid_rows = []
        error_rows = []
        params_list = []
        
        # Get existing emails for duplicate checking
        existing_emails = set()
        if not overwrite_duplicates:
            result = db.fetch_all("SELECT email FROM contacts")
            existing_emails = {row[0].lower() for row in result}  # Convert to lowercase
        
        for row_num, row in enumerate(csv_reader, start=2):
            missing_values = [field for field in required_fields if not row.get(field, '').strip()]
            
            if missing_values:
                error_row = {
                    'Row': row_num
                }
                for field in required_fields_ordered:
                    error_row[field] = row.get(field, '')
                error_row['Error'] = f"Missing values for: {', '.join(missing_values)}"
                error_rows.append(error_row)
                continue

            # Validate ZIP code and get state
            zip_code = row['ZIP Code'].strip()
            zip_info = ZIP_DATA.get(zip_code)
            if not zip_info:
                error_row = {
                    'Row': row_num
                }
                for field in required_fields_ordered:
                    error_row[field] = row.get(field, '')
                error_row['Error'] = f"Invalid ZIP code: {zip_code}"
                error_rows.append(error_row)
                continue

            # Validate gender
            gender = row['Gender'].strip().upper()
            if gender not in ['M', 'F']:
                error_row = {
                    'Row': row_num
                }
                for field in required_fields_ordered:
                    error_row[field] = row.get(field, '')
                error_row['Error'] = f"Invalid gender: {gender}. Must be 'M' or 'F'"
                error_rows.append(error_row)
                continue

            # Check for duplicate email if not overwriting
            email = row['Email'].strip().lower()  # Convert to lowercase
            if not overwrite_duplicates and email in existing_emails:
                error_row = {
                    'Row': row_num
                }
                for field in required_fields_ordered:
                    error_row[field] = row.get(field, '')
                error_row['Error'] = f"Email already exists: {row['Email']}"
                error_rows.append(error_row)
                continue

            try:
                effective_date = datetime.strptime(row['Effective Date'].strip(), '%Y-%m-%d').date()
                birth_date = datetime.strptime(row['Birth Date'].strip(), '%Y-%m-%d').date()
                tobacco_user = row['Tobacco User'].strip().lower() in ['yes', 'true', '1', 'y']

                params_list.append((
                    row['First Name'].strip(),
                    row['Last Name'].strip(),
                    email,  # Use the lowercase email
                    row['Current Carrier'].strip(),
                    row['Plan Type'].strip(),
                    effective_date.isoformat(),
                    birth_date.isoformat(),
                    1 if tobacco_user else 0,
                    gender,
                    zip_info['state'],
                    zip_code
                ))
                valid_rows.append(row)

            except ValueError as e:
                error_row = {
                    'Row': row_num
                }
                for field in required_fields_ordered:
                    error_row[field] = row.get(field, '')
                error_row['Error'] = f"Invalid date format. Dates should be YYYY-MM-DD"
                error_rows.append(error_row)
        
        # Insert valid rows
        inserted_count = 0
        if params_list:
            pprint(params_list)
            if overwrite_duplicates:
                # Use REPLACE INTO or equivalent for your DB
                query = """
                    INSERT OR REPLACE INTO contacts (
                        first_name, last_name, email, current_carrier, plan_type,
                        effective_date, birth_date, tobacco_user, gender,
                        state, zip_code
                    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """
            else:
                query = """
                    INSERT INTO contacts (
                        first_name, last_name, email, current_carrier, plan_type,
                        effective_date, birth_date, tobacco_user, gender,
                        state, zip_code
                    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """
            
            try:
                db.executemany(query, params_list)
                db.connection.commit()
                inserted_count = len(params_list)
                logger.info(f"Successfully inserted {inserted_count} contacts")
            except Exception as e:
                logger.error(f"Error inserting contacts: {e}", exc_info=True)
                return JSONResponse(
                    status_code=200,
                    content={
                        "success": False,
                        "message": f"Error inserting contacts: {str(e)}",
                        "error_csv": None,
                        "total_rows": len(valid_rows) + len(error_rows),
                        "error_rows": len(error_rows) + len(valid_rows),
                        "valid_rows": 0
                    }
                )

        # If there are errors, create an error CSV
        if error_rows:
            output = io.StringIO()
            fieldnames = ['Row'] + required_fields_ordered + ['Error']
            writer = csv.DictWriter(output, fieldnames=fieldnames)
            writer.writeheader()
            for row in error_rows:
                writer.writerow(row)
            
            error_csv = output.getvalue()
            
            return JSONResponse(
                status_code=200,
                content={
                    "success": True,  # Changed to True since we inserted valid rows
                    "message": f"Found {len(error_rows)} rows with errors. Successfully imported {inserted_count} rows.",
                    "error_csv": error_csv,
                    "total_rows": len(valid_rows) + len(error_rows),
                    "error_rows": len(error_rows),
                    "valid_rows": inserted_count
                }
            )
        
        # If no errors, return success
        return {
            "success": True,
            "message": f"Successfully imported {inserted_count} rows",
            "error_csv": None,
            "total_rows": inserted_count,
            "error_rows": 0,
            "valid_rows": inserted_count
        }
        
    except Exception as e:
        logger.error(f"Error processing CSV upload: {e}", exc_info=True)
        return JSONResponse(
            status_code=200,
            content={
                "success": False,
                "message": str(e),
                "error_csv": None,
                "total_rows": 0,
                "error_rows": 0,
                "valid_rows": 0
            }
        ) 

@app.delete("/api/contacts")
def delete_contacts(contact_ids: list[int], db: Database = Depends(get_db)):
    try:
        logger.info(f"Attempting to delete contacts with IDs: {contact_ids}")
        
        # Convert list to comma-separated string for SQL IN clause
        ids_str = ','.join('?' * len(contact_ids))
        
        query = f"""
            DELETE FROM contacts 
            WHERE id IN ({ids_str})
            RETURNING id
        """
        
        result = db.execute(query, tuple(contact_ids))
        deleted_ids = [row[0] for row in result.fetchall()]
        
        logger.info(f"Successfully deleted {len(deleted_ids)} contacts")
        
        return {
            "success": True,
            "deleted_ids": deleted_ids,
            "message": f"Successfully deleted {len(deleted_ids)} contacts"
        }
        
    except Exception as e:
        logger.error(f"Error deleting contacts: {e}", exc_info=True)
        raise HTTPException(status_code=500, detail=str(e)) 