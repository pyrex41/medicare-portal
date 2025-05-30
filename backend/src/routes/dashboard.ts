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
    const effectivePeriod = ['today', 'yesterday', '7days', '30days', '90days', 'ytd'].includes(period || '') ? period : '30days';

    switch (effectivePeriod) {
        case 'today':
            // startDate is already beginning of today
            // endDate is already end of today
            break;
        case 'yesterday':
            startDate.setDate(today.getDate() - 1);
            endDate.setDate(today.getDate() - 1);
            // endDate still needs to be end of that day
            endDate.setHours(23, 59, 59, 999);
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
        let startDateStr: string, endDateStr: string;

        if (period === 'custom' && query?.startDate && query?.endDate) {
            startDateStr = query.startDate as string;
            endDateStr = query.endDate as string;
            // Basic validation for custom dates can be added here if needed
            // For now, assume they are valid YYYY-MM-DD strings
            logger.info(`Using custom date range: ${startDateStr} to ${endDateStr}`);
        } else {
            const range = getDateRange(period);
            startDateStr = range.startDateStr;
            endDateStr = range.endDateStr;
        }
          

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
        
        // 1. "Quotes Sent" - count emails that have been sent (status = 'scheduled') - EXCLUDE follow-ups
        const quotesSentSql = `
          SELECT COUNT(*) as count 
          FROM email_schedules 
          WHERE actual_send_datetime IS NOT NULL
            AND DATE(actual_send_datetime) BETWEEN ? AND ?
            AND (email_type IS NULL OR email_type NOT LIKE 'followup%')
        `
        const quotesSentResult = await orgDb.fetchOne<{ count: number }>(quotesSentSql, [startDateStr, endDateStr])
        const quotesSent = quotesSentResult?.count || 0
        logger.info(`Quotes Sent count: ${quotesSent}`)

        // 1b. "Manual Quotes Sent" - count manual quote emails from email_send_tracking
        const manualQuotesSql = `
          SELECT COUNT(*) as count
          FROM email_send_tracking
          WHERE email_type = 'quote_email' AND send_status IN ('sent', 'delivered', 'scheduled')
            AND DATE(created_at) BETWEEN ? AND ?
        `
        const manualQuotesResult = await orgDb.fetchOne<{ count: number }>(manualQuotesSql, [startDateStr, endDateStr])
        const manualQuotesSent = manualQuotesResult?.count || 0
        logger.info(`Manual Quotes Sent count: ${manualQuotesSent}`)

        // 2. "Quotes Viewed" - count unique contacts who clicked links, attributed to an original send in the period
        const quotesViewedSql = `
          WITH
          original_sends_in_period_for_stats AS (
              SELECT
                  es.contact_id,
                  es.actual_send_datetime AS original_send_datetime_full
              FROM email_schedules es
              WHERE es.actual_send_datetime IS NOT NULL
                AND DATE(es.actual_send_datetime) BETWEEN ? AND ? -- Reporting period for original sends
                AND (es.email_type IS NULL OR es.email_type NOT LIKE 'followup%')
          ),
          clicks_attributed_for_stats AS (
              SELECT
                  tc.contact_id,
                  (
                      SELECT 1 -- We just need to know it's attributable
                      FROM original_sends_in_period_for_stats osip
                      WHERE osip.contact_id = tc.contact_id
                        AND osip.original_send_datetime_full <= tc.clicked_at
                      ORDER BY osip.original_send_datetime_full DESC 
                      LIMIT 1
                  ) AS is_attributable
              FROM tracking_clicks tc
              WHERE tc.contact_id IS NOT NULL AND tc.clicked_at IS NOT NULL
          )
          SELECT COUNT(DISTINCT cas.contact_id) AS count
          FROM clicks_attributed_for_stats cas
          WHERE cas.is_attributable = 1;
        `
        const quotesViewedResult = await orgDb.fetchOne<{ count: number }>(quotesViewedSql, [startDateStr, endDateStr])
        const quotesViewed = quotesViewedResult?.count || 0
        logger.info(`Quotes Viewed count: ${quotesViewed}`)

        // 3. Get "Follow Ups Requested" count from email_schedules (Performance context)
        // These are follow-ups scheduled to be sent in the period.
        const followUpsSql = `
          SELECT COUNT(*) as count
          FROM email_schedules
          WHERE actual_send_datetime IS NOT NULL
            AND DATE(actual_send_datetime) BETWEEN ? AND ?
            AND email_type LIKE 'followup%'
        `
        const followUpsResult = await orgDb.fetchOne<{ count: number }>(followUpsSql, [startDateStr, endDateStr])
        const followUpsRequested = followUpsResult?.count || 0
        logger.info(`Follow Ups Requested count: ${followUpsRequested}`)

        // 4. "Health Questions Completed" - unique contacts who completed health questions during the period
        const healthQuestionsCompletedSql = `
          WITH
          original_sends_in_period_for_stats AS (
              SELECT
                  es.contact_id,
                  es.actual_send_datetime AS original_send_datetime_full
              FROM email_schedules es
              WHERE es.actual_send_datetime IS NOT NULL
                AND DATE(es.actual_send_datetime) BETWEEN ? AND ? -- Reporting period for original sends
                AND (es.email_type IS NULL OR es.email_type NOT LIKE 'followup%')
          ),
          health_completions_attributed_for_stats AS (
              SELECT
                  ea.contact_id,
                  (
                      SELECT 1 -- We just need to know it's attributable
                      FROM original_sends_in_period_for_stats osip
                      WHERE osip.contact_id = ea.contact_id
                        AND osip.original_send_datetime_full <= ea.created_at
                      ORDER BY osip.original_send_datetime_full DESC
                      LIMIT 1
                  ) AS is_attributable
              FROM eligibility_answers ea
              WHERE ea.contact_id IS NOT NULL AND ea.created_at IS NOT NULL
          )
          SELECT COUNT(DISTINCT hcas.contact_id) AS count
          FROM health_completions_attributed_for_stats hcas
          WHERE hcas.is_attributable = 1;
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

        // Total count - EXCLUDE follow-ups
        const upcomingTotalResult = await orgDb.fetchOne<{ total: number } | null>(
          `SELECT COUNT(*) as total FROM email_schedules
           WHERE status = 'pre-scheduled'
             AND scheduled_send_date >= ?
             AND scheduled_send_date <= ?
             AND (email_type IS NULL OR email_type NOT LIKE 'followup%')`,
          [upcomingStartDate, upcomingEndDate]
        );
        const upcomingEmailsTotal = upcomingTotalResult?.total ?? 0;

        // First page (up to 20) - EXCLUDE follow-ups
        const upcomingEmailsPage = await orgDb.fetchAll(
          `SELECT es.id, es.contact_id, es.email_type, es.scheduled_send_date, es.status,
                  c.first_name, c.last_name
           FROM email_schedules es
           LEFT JOIN contacts c ON es.contact_id = c.id
           WHERE es.status = 'pre-scheduled'
             AND es.scheduled_send_date >= ?
             AND es.scheduled_send_date <= ?
             AND (es.email_type IS NULL OR es.email_type NOT LIKE 'followup%')
           ORDER BY es.scheduled_send_date ASC
           LIMIT 30 OFFSET 0`,
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
    // Send ranking endpoint
    .get('/send-ranking', async ({ request, set, query }) => {
      try {
        logger.info('=== SEND RANKING ENDPOINT HIT ===')
        
        const currentUser = await getUserFromSession(request)
        if (!currentUser || !currentUser.organization_id) {
          logger.error('Send ranking: No authenticated user or organization_id')
          set.status = 401
          return {
            success: false,
            error: 'Authentication required'
          }
        }

        logger.info(`Send ranking: User ${currentUser.id} from org ${currentUser.organization_id}`)

        const orgDb = await Database.getOrInitOrgDb(currentUser.organization_id.toString())
        logger.info(`Send ranking: Got org database for ${currentUser.organization_id}`)

        // Always show last 30 days for send ranking, regardless of dropdown
        const today = new Date();
        const thirtyDaysAgo = new Date(today);
        thirtyDaysAgo.setDate(today.getDate() - 29); // Last 30 days including today
        
        const startDateStr = thirtyDaysAgo.toISOString().slice(0, 10);
        const endDateStr = today.toISOString().slice(0, 10);

        logger.info(`Send ranking: Fetching for org ${currentUser.organization_id} for last 30 days (range: ${startDateStr} to ${endDateStr})`);

        // Get daily breakdown of email performance - track emails sent on each date and their outcomes - EXCLUDE follow-ups
        const sendRankingSql = `
          WITH
          original_sends_in_period AS (
              SELECT
                  es.contact_id,
                  DATE(es.actual_send_datetime) AS original_send_date,
                  es.actual_send_datetime AS original_send_datetime_full
              FROM email_schedules es
              WHERE es.actual_send_datetime IS NOT NULL
                AND DATE(es.actual_send_datetime) BETWEEN ? AND ? -- Params: startDateStr, endDateStr
                AND (es.email_type IS NULL OR es.email_type NOT LIKE 'followup%')
          ),
          daily_quotes_sent_agg AS (
              SELECT
                  original_send_date,
                  COUNT(DISTINCT contact_id) AS total_quotes_sent
              FROM original_sends_in_period
              GROUP BY original_send_date
          ),
          clicks_attributed AS (
              SELECT
                  tc.contact_id,
                  (
                      SELECT osip.original_send_date
                      FROM original_sends_in_period osip
                      WHERE osip.contact_id = tc.contact_id
                        AND osip.original_send_datetime_full <= tc.clicked_at -- Original send must be <= click time
                      ORDER BY osip.original_send_datetime_full DESC
                      LIMIT 1
                  ) AS attributable_original_send_date
              FROM tracking_clicks tc
              WHERE tc.contact_id IS NOT NULL AND tc.clicked_at IS NOT NULL
          ),
          daily_quotes_viewed_agg AS (
              SELECT
                  attributable_original_send_date,
                  COUNT(DISTINCT contact_id) AS total_quotes_viewed
              FROM clicks_attributed
              WHERE attributable_original_send_date IS NOT NULL -- Ensures linked to an original send in our reporting period
              GROUP BY attributable_original_send_date
          ),
          health_completions_attributed AS (
              SELECT
                  ea.contact_id,
                  (
                      SELECT osip.original_send_date
                      FROM original_sends_in_period osip
                      WHERE osip.contact_id = ea.contact_id
                        AND osip.original_send_datetime_full <= ea.created_at -- Original send must be <= completion time
                      ORDER BY osip.original_send_datetime_full DESC
                      LIMIT 1
                  ) AS attributable_original_send_date
              FROM eligibility_answers ea
              WHERE ea.contact_id IS NOT NULL AND ea.created_at IS NOT NULL
          ),
          daily_health_completed_agg AS (
              SELECT
                  attributable_original_send_date,
                  COUNT(DISTINCT contact_id) AS total_health_completed
              FROM health_completions_attributed
              WHERE attributable_original_send_date IS NOT NULL
              GROUP BY attributable_original_send_date
          )
          SELECT
              dqs.original_send_date AS sendDate,
              dqs.total_quotes_sent AS quotesSent,
              COALESCE(dqv.total_quotes_viewed, 0) AS quotesViewed,
              COALESCE(dhc.total_health_completed, 0) AS healthCompleted
          FROM daily_quotes_sent_agg dqs
          LEFT JOIN daily_quotes_viewed_agg dqv ON dqs.original_send_date = dqv.attributable_original_send_date
          LEFT JOIN daily_health_completed_agg dhc ON dqs.original_send_date = dhc.attributable_original_send_date
          ORDER BY dqs.original_send_date DESC;
        `;

        logger.info(`Send ranking: Executing SQL with params: [${startDateStr}, ${endDateStr}]`)
        logger.info(`Send ranking: SQL query: ${sendRankingSql}`)

        const sendRankingResult = await orgDb.fetchAll(sendRankingSql, [
          startDateStr, endDateStr
        ]);

        logger.info(`Send ranking: Raw SQL result count: ${sendRankingResult.length}`)
        logger.info(`Send ranking: Raw SQL first 3 results: ${JSON.stringify(sendRankingResult.slice(0, 3))}`)

        // Transform results into the expected format with ranking
        const sendRankingData = sendRankingResult.map((row: any, index: number) => ({
          rank: index + 1,
          sendDate: row.sendDate, // Directly use the aliased column
          quotesSent: parseInt(row.quotesSent || 0), // Directly use the aliased column
          quotesViewed: parseInt(row.quotesViewed || 0), // Directly use the aliased column
          healthCompleted: parseInt(row.healthCompleted || 0) // Directly use the aliased column
        }));

        logger.info(`Send ranking: Transformed data count: ${sendRankingData.length}`)
        logger.info(`Send ranking: Transformed data first 3: ${JSON.stringify(sendRankingData.slice(0, 3))}`)

        logger.info(`Send ranking: Returning response with ${sendRankingData.length} entries`)

        return {
          success: true,
          data: sendRankingData
        }
      } catch (error) {
        logger.error(`=== SEND RANKING ERROR ===`)
        logger.error(`Send ranking error: ${error instanceof Error ? error.message : String(error)}`)
        logger.error(`Send ranking error stack: ${error instanceof Error ? error.stack : 'No stack trace'}`)
        set.status = 500
        return {
          success: false,
          error: 'Failed to load send ranking data'
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
        let startDateStr: string, endDateStr: string;

        if (period === 'custom' && query?.startDate && query?.endDate) {
            startDateStr = query.startDate as string;
            endDateStr = query.endDate as string;
            // Basic validation for custom dates can be added here if needed
             logger.info(`Using custom date range for activity: ${startDateStr} to ${endDateStr}`);
        } else {
            const range = getDateRange(period);
            startDateStr = range.startDateStr;
            endDateStr = range.endDateStr;
        }

        logger.info(`Fetching dashboard activity stats for org ${currentUser.organization_id} for period: ${period || '30days'} (range: ${startDateStr} to ${endDateStr})`);

        // 0. "Total Emails Sent" - unique contacts who were sent an email in the period
        const emailsSentSql = `
          SELECT COUNT(DISTINCT contact_id) as count
          FROM email_schedules
          WHERE contact_id IS NOT NULL
            AND status IN ('scheduled', 'send', 'delivered')
            AND scheduled_send_date BETWEEN ? AND ?
        `;
        const emailsSentResult = await orgDb.fetchOne<{ count: number }>(emailsSentSql, [startDateStr, endDateStr])
        const emailsSent = emailsSentResult?.count || 0
        logger.info(`Emails Sent (Activity, unique contacts) count: ${emailsSent}`)

        // 1. "Links Clicked" - count unique contacts who clicked in the period (no double counting)
        const linksClickedSql = `
          SELECT COUNT(DISTINCT contact_id) as count
          FROM tracking_clicks
          WHERE contact_id IS NOT NULL
            AND DATE(clicked_at) BETWEEN ? AND ?
        `;
        const linksClickedResult = await orgDb.fetchOne<{ count: number }>(linksClickedSql, [startDateStr, endDateStr])
        const linksClicked = linksClickedResult?.count || 0
        logger.info(`Links Clicked (Activity, unique contacts) count: ${linksClicked}`)

        // 2. "Health Questions Completed" - unique contacts who completed eligibility_answers in the period
        const healthQuestionsCompletedActivitySql = `
          SELECT COUNT(DISTINCT contact_id) as count
          FROM eligibility_answers
          WHERE contact_id IS NOT NULL
            AND DATE(created_at) BETWEEN ? AND ?
        `;
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
