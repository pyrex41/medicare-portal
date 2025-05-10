import { Elysia } from 'elysia'
import { Database } from '../database'
import { logger } from '../logger'
import { getUserFromSession } from '../services/auth'

/**
 * Creates and configures dashboard-related routes
 */
export function createDashboardRoutes() {
  return new Elysia({ prefix: '/api/dashboard' })
    // Dashboard statistics endpoint
    .get('/stats', async ({ request, set }) => {
      try {
        // Get current user from session
        const currentUser = await getUserFromSession(request)
        if (!currentUser || !currentUser.organization_id) {
          set.status = 401
          return {
            success: false,
            error: 'Authentication required'
          }
        }

        // Get organization-specific database
        const orgDb = await Database.getOrInitOrgDb(currentUser.organization_id.toString())

        // For accurate counts, we need to be precise about what we're counting
        // Log all tables to help diagnose
        logger.info(`Checking available tables in org DB for ${currentUser.organization_id}`)
        const tablesResult = await orgDb.fetchAll(
          "SELECT name FROM sqlite_master WHERE type='table'"
        )
        logger.info(`Available tables: ${JSON.stringify(tablesResult.map((r: any) => r.name || r[0]))}`)
        
        // Better debug query to understand tracking_clicks structure
        const sampleClicks = await orgDb.fetchAll(
          "SELECT * FROM tracking_clicks LIMIT 5"
        )
        logger.info(`Sample tracking_clicks: ${JSON.stringify(sampleClicks)}`)
        
        // 1. "Quotes Sent" - count emails that have been sent (status = 'scheduled')
        const quotesSentSql = `
          SELECT COUNT(*) as count 
          FROM email_schedules 
          WHERE status = 'scheduled'
        `
        const quotesSentResult = await orgDb.fetchOne<{ count: number }>(quotesSentSql)
        const quotesSent = quotesSentResult?.count || 0
        logger.info(`Quotes Sent count: ${quotesSent}`)

        // 2. "Quotes Viewed" - count unique contacts who have viewed quotes
        const quotesViewedSql = `
          SELECT COUNT(DISTINCT contact_id) as count 
          FROM tracking_clicks
          WHERE contact_id IS NOT NULL
        `
        const quotesViewedResult = await orgDb.fetchOne<{ count: number }>(quotesViewedSql)
        const quotesViewed = quotesViewedResult?.count || 0
        logger.info(`Quotes Viewed count: ${quotesViewed}`)

        // 3. Get "Upcoming Renewals" count from email_schedules
        const followUpsSql = `
          SELECT COUNT(*) as count 
          FROM email_schedules 
          WHERE status = 'pre-scheduled'
        `
        const followUpsResult = await orgDb.fetchOne<{ count: number }>(followUpsSql)
        const followUpsRequested = followUpsResult?.count || 0

        // Generate synthetic chart data for past 12 months, with current month in middle
        const currentMonth = new Date().getMonth(); // 0-11
        
        // Create array of dummy chart data for each month
        const chartData = [];
        const monthsToShow = 12;
        
        // Generate data for each month, starting 6 months ago and showing 12 months total
        const startMonth = (currentMonth - 6 + 12) % 12; // Go back 6 months, wrap around 
        
        for (let i = 0; i < monthsToShow; i++) {
          const monthIndex = (startMonth + i) % 12;
          
          // Make current month have some activity
          const isCurrentMonth = monthIndex === currentMonth;
          
          // Scale activity based on proximity to current month
          const proximity = Math.abs(i - 6) / 6; // 0 = current month, 1 = farthest month
          const randomFactor = Math.random() * 0.5 + 0.5; // 0.5-1.0 random factor
          
          let sentValue = 0;
          let viewedValue = 0;
          let followUpValue = 0;
          
          if (isCurrentMonth) {
            // Use real data for current month
            sentValue = quotesSent;
            viewedValue = quotesViewed / 2; // Reduce to make chart more readable
            followUpValue = followUpsRequested;
          } else {
            // Synthetic data for other months, diminishing with distance
            sentValue = Math.round((quotesSent * 0.8 * (1 - proximity) * randomFactor));
            viewedValue = Math.round((quotesViewed * 0.4 * (1 - proximity) * randomFactor));
            followUpValue = Math.round((followUpsRequested * 0.6 * (1 - proximity) * randomFactor));
          }
          
          chartData.push({
            x: i, // 0-11 for the months, starting with 0 = 6 months ago
            sends: sentValue,
            views: viewedValue, 
            followUps: followUpValue
          });
        }

        // No need to transform the chart data anymore

        logger.info(`Dashboard stats for org ${currentUser.organization_id}: Quotes Sent: ${quotesSent}, Quotes Viewed: ${quotesViewed}, Renewals: ${followUpsRequested}`)

        return {
          success: true,
          stats: {
            quotesSent,
            quotesViewed,
            followUpsRequested,
            chartData
          }
        }
      } catch (error) {
        logger.error(`Error fetching dashboard stats: ${error}`)
        set.status = 500
        return {
          success: false,
          error: 'Failed to load dashboard stats'
        }
      }
    })
}