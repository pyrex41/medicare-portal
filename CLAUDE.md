# Medicare Portal Development Guidelines

## Build Commands
- Backend: `cd backend && bun run dev` - starts backend server with file watching
- Frontend: `cd frontend && bunx --bun vite` - starts frontend dev server
- Build frontend: `cd frontend && tsc && bunx --bun vite build`
- Add agent: `cd backend && bun run add-agent`
- Manage recommendations: `cd backend && bun run manage-recommendations`

## Code Style
- TypeScript: Use strict mode, prefer explicit types
- Frontend: Elm for main app logic, TypeScript for interop
- Imports: Group by external/internal, alphabetize
- Naming: camelCase for variables/functions, PascalCase for types/components
- Error handling: Use explicit error types from errors.ts
- File organization: Feature-based folders in src/

## Architecture
- Backend: Elysia.js (Bun) with Turso/LibSQL database
- Frontend: Elm with TailwindCSS
- Authentication: Cookie-based with middleware/auth.ts

## Important Rule
- Do not restart frontend or backend servers - file watchers are active