# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

# Medicare Portal Development Guidelines

## Build Commands
- Backend: `cd backend && bun run dev` - starts backend server with file watching
- Frontend: `cd frontend && bunx --bun vite` - starts frontend dev server
- Build frontend: `cd frontend && tsc && bunx --bun vite build`
- Add agent: `cd backend && bun run add-agent`
- Manage recommendations: `cd backend && bun run manage-recommendations`
- Manage Turso databases: `cd backend && bun run manage-turso`
- Test bulk import: `cd backend && bun run test-import`
- Generate test data: `cd backend && bun run generate-data`
- Fix contacts: `cd backend && bun run fix-contacts`
- Start production server: `cd backend && bun run start`

## Architecture

### Backend
- Runtime: Bun with Elysia.js framework
- Database: Turso/LibSQL (SQLite in the cloud)
- Database structure:
  - Main central database for users, organizations, and auth
  - Per-organization databases for contacts and related data
- Authentication: Cookie-based sessions (middleware/auth.ts)
- Key services:
  - auth.ts: User authentication and sessions
  - email.ts: Email sending via SendGrid
  - stripe.ts: Payment processing
  - turso.ts: Database management
  - contactTracking.ts: Contact activity tracking

### Frontend
- Framework: Elm for the main application logic
- Build: Vite with TypeScript for configuration
- Styling: TailwindCSS
- JavaScript interop: Through Elm ports (Ports.elm)
- Key modules:
  - Main.elm: Main application entry point
  - Components/: Reusable UI components
  - Utils/: Utilities and helper functions

### Database Schema
- Organizations have users (agents/admins)
- Each organization has its own database for contacts
- Contacts can be imported via CSV
- Tracking for contact interactions and email activity

## Code Style
- TypeScript: Use strict mode, prefer explicit types
- Frontend: Elm for main app logic, TypeScript for interop
- Imports: Group by external/internal, alphabetize
- Naming: camelCase for variables/functions, PascalCase for types/components
- Error handling: Use explicit error types from errors.ts
- File organization: Feature-based folders in src/

## Key Project Features
- Medicare plan comparison and quoting
- Contact management system
- Agent management and licensing
- Email automation and scheduling
- Payment processing with Stripe
- Bulk contact import

## Important Rules
- Do not restart frontend or backend servers - file watchers are active
- Always use transactions for multi-step database operations
- For database operations on organization databases, use Database.getOrgDb or Database.getOrInitOrgDb
- Check for proper authentication in API routes
- Handle errors consistently with the errorHandler middleware