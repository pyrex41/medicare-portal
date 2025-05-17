import { Elysia } from 'elysia'
import { Database } from '../database'
import { logger } from '../logger'
import { getUserFromSession } from '../services/auth'

// Helper function to get date range for SQL queries
function getDateRange(period?: string): { startDateStr: string, endDateStr: string } {
    const today = new Date();
    let startDate = new Date(today);
    let endDate = new Date(today);

    startDate.setHours(0, 0, 0, 0);
    endDate.setHours(23, 59, 59, 999); // End of today for endDate default

    // Default to '30days' if period is undefined or invalid
    const effectivePeriod = ['today', '7days', '30days', '90days', 'ytd'].includes(period || '') ? period : '30days';

    switch (effectivePeriod) {
        case 'today':
            // startDate is already beginning of today
            // endDate is already end of today
            break;
        case '7days':
            startDate.setDate(today.getDate() - 6); // today is the 7th day
            break;
        case '90days':
            startDate.setDate(today.getDate() - 89); // today is the 90th day
            break;
        case 'ytd':
            startDate = new Date(today.getFullYear(), 0, 1); // First day of current year
            startDate.setHours(0, 0, 0, 0);
            break;
        case '30days':
        default:
            startDate.setDate(today.getDate() - 29); // today is the 30th day
            break;
    }
    const formatDate = (d: Date) => d.toISOString().slice(0, 10); // YYYY-MM-DD
    return {
        startDateStr: formatDate(startDate),
        endDateStr: formatDate(endDate), // endDate is always today's date
    };
}

/**
 * Creates and configures dashboard-related routes
 */
export function createDashboardRoutes() {
  return new Elysia({ prefix: '/api/dashboard' })
    // Dashboard statistics endpoint
    .get('/stats', async ({ request, set, query }) => {
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

        const period = query?.period as string | undefined;
        const { startDateStr, endDateStr } = getDateRange(period);

        logger.info(`Fetching dashboard stats for org ${currentUser.organization_id} for period: ${period || '30days'} (range: ${startDateStr} to ${endDateStr})`);

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
            AND scheduled_send_date BETWEEN ? AND ?
        `
        const quotesSentResult = await orgDb.fetchOne<{ count: number }>(quotesSentSql, [startDateStr, endDateStr])
        const quotesSent = quotesSentResult?.count || 0
        logger.info(`Quotes Sent count: ${quotesSent}`)

        // 1b. "Manual Quotes Sent" - count manual quote emails from email_send_tracking
        const manualQuotesSql = `
          SELECT COUNT(*) as count
          FROM email_send_tracking
          WHERE email_type = 'quote_email' AND send_status = 'sent'
            AND DATE(created_at) BETWEEN ? AND ?
        `
        const manualQuotesResult = await orgDb.fetchOne<{ count: number }>(manualQuotesSql, [startDateStr, endDateStr])
        const manualQuotesSent = manualQuotesResult?.count || 0
        logger.info(`Manual Quotes Sent count: ${manualQuotesSent}`)

        // 2. "Quotes Viewed" (Performance) - count unique contacts who received a scheduled email in the period and have any click in tracking_clicks
        const quotesViewedSql = `
          SELECT COUNT(DISTINCT es.contact_id) as count 
          FROM email_schedules es
          JOIN tracking_clicks tc ON es.contact_id = tc.contact_id
          WHERE es.status = 'scheduled'
            AND es.scheduled_send_date BETWEEN ? AND ?
        `
        const quotesViewedResult = await orgDb.fetchOne<{ count: number }>(quotesViewedSql, [startDateStr, endDateStr])
        const quotesViewed = quotesViewedResult?.count || 0
        logger.info(`Quotes Viewed (Performance) count: ${quotesViewed}`)

        // 3. Get "Follow Ups Requested" count from email_schedules (Performance context)
        // These are follow-ups scheduled to be sent in the period.
        const followUpsSql = `
          SELECT COUNT(*) as count
          FROM email_schedules
          WHERE status = 'scheduled' -- Assuming 'scheduled' means sent for past dates
            AND email_type LIKE 'follow_up_%' 
            AND scheduled_send_date BETWEEN ? AND ?
        `
        const followUpsResult = await orgDb.fetchOne<{ count: number }>(followUpsSql, [startDateStr, endDateStr])
        const followUpsRequested = followUpsResult?.count || 0
        logger.info(`Follow Ups Requested count: ${followUpsRequested}`)

        // 4. Get "Health Questions Completed" count (Performance) - unique contacts who received a scheduled email in the period and completed eligibility_answers
        const healthQuestionsCompletedSql = `
          SELECT COUNT(DISTINCT es.contact_id) as count
          FROM email_schedules es
          JOIN eligibility_answers ea ON es.contact_id = ea.contact_id
          WHERE es.status = 'scheduled'
            AND es.scheduled_send_date BETWEEN ? AND ?
        `
        const healthQuestionsResult = await orgDb.fetchOne<{ count: number }>(healthQuestionsCompletedSql, [startDateStr, endDateStr])
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
          let healthCompletedValue = 0;

          if (isCurrentMonth) {
            // Use real data for current month
            sentValue = quotesSent;
            viewedValue = quotesViewed;
            followUpValue = followUpsRequested;
            healthCompletedValue = healthQuestionsCompleted;
          }

          chartData.push({
            x: i, // 0-11 for months (Jan-Dec)
            sends: sentValue,
            views: viewedValue,
            followUps: followUpValue,
            healthCompleted: healthCompletedValue
          });
        }

        // No need to transform the chart data anymore

        logger.info(`Dashboard stats for org ${currentUser.organization_id} (period ${period}): Quotes Sent: ${quotesSent}, Manual Quotes Sent: ${manualQuotesSent}, Quotes Viewed: ${quotesViewed}, FollowUps: ${followUpsRequested}, HealthCompleted: ${healthQuestionsCompleted}`)

        // UPCOMING EMAILS LOGIC
        // Only include emails scheduled in the next 30 days and status = 'pre-scheduled'
        const now = new Date();
        const in30Days = new Date(now);
        in30Days.setDate(now.getDate() + 30);
        const upcomingStartDate = now.toISOString().slice(0, 10);
        const upcomingEndDate = in30Days.toISOString().slice(0, 10);

        // Total count
        const upcomingTotalResult = await orgDb.fetchOne<{ total: number } | null>(
          `SELECT COUNT(*) as total FROM email_schedules
           WHERE status = 'pre-scheduled'
             AND scheduled_send_date >= ?
             AND scheduled_send_date <= ?`,
          [upcomingStartDate, upcomingEndDate]
        );
        const upcomingEmailsTotal = upcomingTotalResult?.total ?? 0;

        // First page (up to 20)
        const upcomingEmailsPage = await orgDb.fetchAll(
          `SELECT es.id, es.contact_id, es.email_type, es.scheduled_send_date, es.status,
                  c.first_name, c.last_name
           FROM email_schedules es
           LEFT JOIN contacts c ON es.contact_id = c.id
           WHERE es.status = 'pre-scheduled'
             AND es.scheduled_send_date >= ?
             AND es.scheduled_send_date <= ?
           ORDER BY es.scheduled_send_date ASC
           LIMIT 20 OFFSET 0`,
          [upcomingStartDate, upcomingEndDate]
        );

        return {
          success: true,
          stats: {
            quotesSent,
            manualQuotesSent,
            quotesViewed,
            followUpsRequested,
            healthQuestionsCompleted,
            chartData,
            upcomingEmailsTotal,
            upcomingEmailsPage
          }
        }
      } catch (error) {
        logger.error(`Error fetching dashboard stats: ${error instanceof Error ? error.message : String(error)}`)
        set.status = 500
        return {
          success: false,
          error: 'Failed to load dashboard stats'
        }
      }
    })
}

/**
 * Creates and configures dashboard activity-related routes
 */
export function createDashboardActivityRoutes() {
  return new Elysia({ prefix: '/api/dashboard' })
    .get('/activity', async ({ request, set, query }) => {
      try {
        const currentUser = await getUserFromSession(request)
        if (!currentUser || !currentUser.organization_id) {
          set.status = 401
          return {
            success: false,
            error: 'Authentication required'
          }
        }

        const orgDb = await Database.getOrInitOrgDb(currentUser.organization_id.toString())

        const period = query?.period as string | undefined;
        const { startDateStr, endDateStr } = getDateRange(period);

        logger.info(`Fetching dashboard activity stats for org ${currentUser.organization_id} for period: ${period || '30days'} (range: ${startDateStr} to ${endDateStr})`);

        // 0. "Total Emails Sent" - unique contacts who were sent an email in the period
        const emailsSentSql = `
          SELECT COUNT(DISTINCT contact_id) as count
          FROM email_schedules
          WHERE contact_id IS NOT NULL
            AND status = 'scheduled'
            AND scheduled_send_date BETWEEN ? AND ?
        `
        const emailsSentResult = await orgDb.fetchOne<{ count: number }>(emailsSentSql, [startDateStr, endDateStr])
        const emailsSent = emailsSentResult?.count || 0
        logger.info(`Emails Sent (Activity, unique contacts) count: ${emailsSent}`)

        // 1. "Links Clicked" - count unique contacts who clicked in the period (no double counting)
        const linksClickedSql = `
          SELECT COUNT(DISTINCT contact_id) as count
          FROM tracking_clicks
          WHERE contact_id IS NOT NULL
            AND DATE(clicked_at) BETWEEN ? AND ?
        `
        const linksClickedResult = await orgDb.fetchOne<{ count: number }>(linksClickedSql, [startDateStr, endDateStr])
        const linksClicked = linksClickedResult?.count || 0
        logger.info(`Links Clicked (Activity, unique contacts) count: ${linksClicked}`)

        // 2. "Health Questions Completed" - unique contacts who completed eligibility_answers in the period
        const healthQuestionsCompletedActivitySql = `
          SELECT COUNT(DISTINCT contact_id) as count
          FROM eligibility_answers
          WHERE contact_id IS NOT NULL
            AND DATE(created_at) BETWEEN ? AND ?
        `
        const healthQuestionsCompletedActivityResult = await orgDb.fetchOne<{ count: number }>(healthQuestionsCompletedActivitySql, [startDateStr, endDateStr])
        const healthQuestionsCompletedActivity = healthQuestionsCompletedActivityResult?.count || 0
        logger.info(`Health Questions Completed (Activity, unique contacts) count: ${healthQuestionsCompletedActivity}`)
        
        // For the activity tab, chartData might be simpler or not used extensively initially.
        // We can return raw values that could be used for simple bar charts if needed.
        const activityChartData = [
          {
            x: 0, // Representing Emails Sent
            value: emailsSent
          },
          {
            x: 1, // Representing Links Clicked
            value: linksClicked 
          },
          {
            x: 2, // Representing Health Questions Completed
            value: healthQuestionsCompletedActivity
          }
        ];
        set.status = 200
        return {
          success: true,
          stats: {
            emailsSent,
            linksClicked,
            healthQuestionsCompleted: healthQuestionsCompletedActivity,
            activityChartData // Sending a simple chart data structure
          }
        }
      } catch (error) {
        logger.error(`Error fetching dashboard activity stats: ${error instanceof Error ? error.message : String(error)}`)
        set.status = 500
        return {
          success: false,
          error: 'Failed to load dashboard activity stats'
        }
      }
    })
}

// Make sure to integrate this new route group in your main server setup, similar to createDashboardRoutes.
// For example, in src/index.ts or where Elysia app is instantiated:
// app.use(createDashboardRoutes())
//    .use(createDashboardActivityRoutes()) // Add this line