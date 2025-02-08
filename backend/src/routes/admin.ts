import { Elysia } from 'elysia'
import { staticPlugin } from '@elysiajs/static'
import { spawn } from 'child_process'
import path from 'path'
import { TursoService } from '../services/turso'

const tursoService = new TursoService()

export const adminRoutes = new Elysia()
  // Protect admin routes
  .guard({
    beforeHandle: ({ request, set }) => {
      // Add your admin authentication logic here
      const isAdmin = true // Replace with actual admin check
      if (!isAdmin) {
        set.status = 401
        return 'Unauthorized'
      }
    }
  })
  // Serve admin portal static files
  .use(staticPlugin({
    assets: '../admin-portal/dist',
    prefix: '/admin'
  }))
  // Terminal WebSocket endpoint
  .ws('/admin/terminal', {
    open(ws) {
      ws.send('Connected to admin terminal')
    },
    message(ws, message) {
      const { command, args } = JSON.parse(message as string)
      
      const scripts = {
        'add-agent': '../scripts/add-agent.ts',
        'manage-recommendations': '../scripts/manage-recommendations.ts'
      }

      if (!scripts[command]) {
        ws.send(JSON.stringify({ error: 'Invalid command' }))
        return
      }

      const script = spawn('bun', [path.resolve(__dirname, scripts[command]), ...args], {
        stdio: ['pipe', 'pipe', 'pipe']
      })

      script.stdout.on('data', (data) => {
        ws.send(JSON.stringify({ type: 'output', data: data.toString() }))
      })

      script.stderr.on('data', (data) => {
        ws.send(JSON.stringify({ type: 'error', data: data.toString() }))
      })

      script.on('close', (code) => {
        ws.send(JSON.stringify({ type: 'exit', code }))
      })
    }
  })

// In your create organization endpoint:
app.post('/api/organizations', async (req, res) => {
  const { name, /* other org fields */ } = req.body
  
  try {
    // Create org in central DB
    const org = await db.one(`
      INSERT INTO organizations (name) 
      VALUES ($1) 
      RETURNING id`, 
      [name]
    )

    // Create Turso DB for org
    const { url, token } = await tursoService.createOrganizationDatabase(org.id)

    // Update org with Turso credentials
    await db.none(`
      UPDATE organizations 
      SET turso_db_url = $1, turso_auth_token = $2
      WHERE id = $3`,
      [url, token, org.id]
    )

    res.json({ success: true, organization: org })
  } catch (err) {
    res.status(500).json({ success: false, error: err.message })
  }
}) 