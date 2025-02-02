from fastapi import FastAPI, Request, Depends
from fastapi.templating import Jinja2Templates
from fastapi.staticfiles import StaticFiles
from db.database import get_db, Database
from models.contact import ContactCreate
import logging

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

app = FastAPI()

# Mount static files
app.mount("/static", StaticFiles(directory="static"), name="static")

# Set up Jinja2 templates
templates = Jinja2Templates(directory="templates")

@app.get("/")
def home(request: Request, db: Database = Depends(get_db)):
    try:
        # Fetch contacts from database
        logger.info("Attempting to fetch contacts")
        contacts = db.fetch_all(
            "SELECT * FROM contacts ORDER BY created_at DESC LIMIT 100"
        )
        logger.info(f"Successfully fetched {len(contacts)} contacts")
        
        return templates.TemplateResponse(
            "index.html",
            {"request": request, "title": "Medicare Portal", "contacts": contacts}
        )
    except Exception as e:
        logger.error(f"Error in home route: {e}", exc_info=True)
        # Re-raise to let FastAPI handle the error response
        raise

@app.post("/api/contacts")
def create_contact(contact: ContactCreate, db: Database = Depends(get_db)):
    try:
        logger.info(f"Attempting to create contact: {contact.first_name} {contact.last_name}")
        query = """
            INSERT INTO contacts (
                first_name, last_name, current_carrier, plan_type,
                effective_date, birth_date, tobacco_user, gender,
                state, zip_code
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            RETURNING *
        """
        # Convert dates to strings and boolean to integer
        params = (
            contact.first_name,
            contact.last_name,
            contact.current_carrier,
            contact.plan_type,
            contact.effective_date.isoformat(),  # Convert date to string
            contact.birth_date.isoformat(),      # Convert date to string
            1 if contact.tobacco_user else 0,    # Convert bool to int
            contact.gender,
            contact.state,
            contact.zip_code,
        )
        
        logger.info("Executing insert query")
        result = db.execute(query, params)
        row = result.fetchone()  # Get the actual row data
        logger.info("Insert successful")
        return dict(zip(  # Convert row tuple to a dictionary
            ['id', 'first_name', 'last_name', 'current_carrier', 'plan_type', 
             'effective_date', 'birth_date', 'tobacco_user', 'gender', 'state', 
             'zip_code', 'last_emailed_date', 'created_at', 'updated_at'], 
            row
        ))
    except Exception as e:
        logger.error(f"Error creating contact: {e}", exc_info=True)
        raise

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