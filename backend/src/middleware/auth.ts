import { Elysia } from 'elysia'
import { logger } from '../logger'
import { getUserFromSession } from '../services/auth'

// Auth middleware for protected routes
export const requireAuth = new Elysia()
  .derive(async ({ request, set }) => {
    try {
      // Check for auth bypass header - bypass if present
      const bypassAuth = request.headers.get('X-Bypass-Auth')
      if (bypassAuth) {
        logger.info(`Auth middleware: bypassing auth due to X-Bypass-Auth header`)
        return
      }

      // Check if this is a public endpoint that should bypass auth
      const url = new URL(request.url)
      const pathname = url.pathname
      
      // Bypass auth for SPA routes - belt and suspenders approach
      if ((!pathname.startsWith('/api/') && !pathname.includes('.')) || 
          pathname.startsWith('/compare/') ||
          pathname.startsWith('/quote/') ||
          pathname.startsWith('/eligibility') ||
          pathname.startsWith('/schedule')) {
        logger.info(`Auth middleware: bypassing auth for SPA route: ${pathname}`)
        return
      }
      
      // Get user from session
      const user = await getUserFromSession(request)
      
      // If no user or skip_auth not set, return 401
      if (!user || !(user as any).skip_auth) {
        logger.warn(`Auth middleware: unauthorized access to ${pathname}`)
        set.status = 401
        return { success: false, error: 'Authentication required' }
      }
      
      // Return user object for routes to use
      return { user }
    } catch (error) {
      logger.error(`Auth middleware error: ${error}`)
      set.status = 500
      return { success: false, error: 'Internal server error' }
    }
  })

// Admin middleware - requires auth first
export const requireAdmin = new Elysia()
  .derive(async ({ request, set }) => {
    try {
      // Check for auth bypass header - bypass if present
      const bypassAuth = request.headers.get('X-Bypass-Auth')
      if (bypassAuth) {
        logger.info(`Admin middleware: bypassing auth due to X-Bypass-Auth header`)
        return
      }
      
      // Get user from session
      const user = await getUserFromSession(request)
      
      // If no user or not admin, return 403
      if (!user || !user.is_admin) {
        logger.warn(`Admin middleware: forbidden access to ${new URL(request.url).pathname}`)
        set.status = 403
        return { success: false, error: 'Admin access required' }
      }
      
      // Return user object for routes to use
      return { user }
    } catch (error) {
      logger.error(`Admin middleware error: ${error}`)
      set.status = 500
      return { success: false, error: 'Internal server error' }
    }
  }) 