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

        // 1b. "Manual Quotes Sent" - count manual quote emails from email_send_tracking
        const manualQuotesSql = `
          SELECT COUNT(*) as count
          FROM email_send_tracking
          WHERE email_type = 'quote_email' AND send_status = 'sent'
        `;
        const manualQuotesResult = await orgDb.fetchOne<{ count: number }>(manualQuotesSql);
        const manualQuotesSent = manualQuotesResult?.count || 0;
        logger.info(`Manual Quotes Sent count: ${manualQuotesSent}`);

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

        // 4. Get "Health Questions Completed" count - unique contacts who have completed health questions
        const healthQuestionsCompletedSql = `
          SELECT COUNT(DISTINCT contact_id) as count
          FROM eligibility_answers
        `
        const healthQuestionsResult = await orgDb.fetchOne<{ count: number }>(healthQuestionsCompletedSql)
        const healthQuestionsCompleted = healthQuestionsResult?.count || 0
        logger.info(`Health Questions Completed count: ${healthQuestionsCompleted}`)

        // Show data for all 12 months, with real data for current month and zeros for others
        const currentMonth = new Date().getMonth(); // 0-11

        // Create array of chart data for all months
        const chartData = [];
        const monthsToShow = 12;

        // Create entries for all 12 months, but only put real data in current month
        for (let i = 0; i < monthsToShow; i++) {
          // Only the current month has actual data, rest are zeros
          const isCurrentMonth = i === currentMonth;

          let sentValue = 0;
          let viewedValue = 0;
          let followUpValue = 0;

          if (isCurrentMonth) {
            // Use real data for current month
            sentValue = quotesSent;
            viewedValue = quotesViewed;
            followUpValue = followUpsRequested;
          }

          chartData.push({
            x: i, // 0-11 for months (Jan-Dec)
            sends: sentValue,
            views: viewedValue,
            followUps: followUpValue
          });
        }

        // No need to transform the chart data anymore

        logger.info(`Dashboard stats for org ${currentUser.organization_id}: Quotes Sent: ${quotesSent}, Manual Quotes Sent: ${manualQuotesSent}, Quotes Viewed: ${quotesViewed}, Renewals: ${followUpsRequested}`)

        return {
          success: true,
          stats: {
            quotesSent,
            manualQuotesSent,
            quotesViewed,
            followUpsRequested,
            healthQuestionsCompleted,
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