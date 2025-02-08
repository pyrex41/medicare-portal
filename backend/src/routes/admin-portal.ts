import { Elysia } from 'elysia'
import { spawn } from 'child_process'
import path from 'path'
import { logger } from '../logger'
import { requireAuth, requireAdmin } from '../middleware/auth'

// Create admin portal routes
export const createAdminPortalRoutes = () => {
  const adminPortal = new Elysia({ prefix: '/api/admin' })
    .use(requireAuth)
    .use(requireAdmin)

    // Get available admin scripts
    .get('/scripts', () => {
      const availableScripts = [
        {
          id: 'add-agent',
          name: 'Add Agent',
          description: 'Interactive tool to add new agents',
          script: 'add-agent.ts'
        },
        {
          id: 'manage-recommendations',
          name: 'Manage GI Recommendations', 
          description: 'Manage guaranteed issue recommendations',
          script: 'manage-recommendations.ts'
        }
      ]
      
      return { scripts: availableScripts }
    })

    // Execute script endpoint
    .post('/execute/:scriptId', ({ params, set, server }) => {
      const { scriptId } = params
      const scriptPath = path.join(__dirname, '../../scripts', `${scriptId}.ts`)

      logger.info(`Executing admin script: ${scriptId}`)

      // Spawn script process
      const process = spawn('bun', ['run', scriptPath], {
        stdio: ['pipe', 'pipe', 'pipe']
      })

      // Get WebSocket server instance
      const wss = server.webSocket

      // Handle new WebSocket connections
      wss.on('connection', (ws) => {
        logger.info('Admin terminal WebSocket connected')

        // Handle input from client
        ws.on('message', (data: string) => {
          const parsed = JSON.parse(data)
          if (parsed.type === 'input') {
            process.stdin.write(parsed.data)
          }
        })

        // Send output to client
        process.stdout.on('data', (data) => {
          ws.send(JSON.stringify({ 
            type: 'output', 
            data: data.toString() 
          }))
        })

        process.stderr.on('data', (data) => {
          ws.send(JSON.stringify({ 
            type: 'error', 
            data: data.toString() 
          }))
        })

        // Handle process exit
        process.on('exit', (code) => {
          ws.send(JSON.stringify({ type: 'exit', code }))
          logger.info(`Admin script ${scriptId} exited with code ${code}`)
        })

        // Handle WebSocket close
        ws.on('close', () => {
          logger.info('Admin terminal WebSocket disconnected')
          process.kill()
        })
      })

      set.status = 200
      return { 
        success: true, 
        message: 'Script execution started' 
      }
    })

  return adminPortal
} 