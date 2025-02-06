import { Elysia } from 'elysia'
import { getAgentSettings, updateAgentSettings } from '../services/agentSettings'
import { UnauthorizedError } from '../errors'
import { db } from '../database'

export const settingsRoutes = (app: Elysia) =>
  app
    .get('/api/settings', async ({ user }) => {
      if (!user?.id) {
        throw new UnauthorizedError('No authenticated user')
      }

      // Verify user is an active agent
      const isActiveAgent = await db.fetchOne(
        'SELECT 1 FROM users WHERE id = ? AND is_active = true AND role = ?',
        [user.id, 'agent']
      )

      if (!isActiveAgent) {
        throw new UnauthorizedError('User is not an active agent')
      }

      const settings = await getAgentSettings(user.id)
      return settings
    })
    .put('/api/settings', async ({ user, body }) => {
      if (!user?.id) {
        throw new UnauthorizedError('No authenticated user')
      }

      // Verify user is an active agent
      const isActiveAgent = await db.fetchOne(
        'SELECT 1 FROM users WHERE id = ? AND is_active = true AND role = ?',
        [user.id, 'agent']
      )

      if (!isActiveAgent) {
        throw new UnauthorizedError('User is not an active agent')
      }

      const settings = await updateAgentSettings(user.id, body)
      return settings
    }) 