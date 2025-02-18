import { Elysia } from 'elysia';
import { Database } from '../database';
import { logger } from '../logger';

const db = new Database();

interface Organization {
  id: number;
}

interface BrandSettings {
  brand_name: string;
  primary_color: string;
  secondary_color: string;
  logo_data: string | null;
}

export function createBrandRoutes() {
  return new Elysia()
    .get('/api/organizations/:orgSlug/brand', async ({ params }) => {
      try {
        const { orgSlug } = params;

        // Get organization ID
        const org = await db.fetchOne<Organization>(
          'SELECT id FROM organizations WHERE slug = ?',
          [orgSlug]
        );

        if (!org) {
          return {
            success: false,
            error: 'Organization not found'
          };
        }

        // Get brand settings
        const settings = await db.fetchOne<BrandSettings>(
          'SELECT brand_name, primary_color, secondary_color, logo_data FROM brand_settings WHERE organization_id = ?',
          [org.id]
        );

        if (!settings) {
          // Return defaults if no settings exist
          return {
            brand_name: '',
            primary_color: '#6B46C1',
            secondary_color: '#9F7AEA',
            logo: null
          };
        }

        return {
          brand_name: settings.brand_name,
          primary_color: settings.primary_color,
          secondary_color: settings.secondary_color,
          logo: settings.logo_data
        };
      } catch (error) {
        logger.error('Error fetching brand settings', error);
        return {
          success: false,
          error: 'Failed to fetch brand settings'
        };
      }
    })
    .put('/api/organizations/:orgSlug/brand', async ({ params, body }) => {
      try {
        const { orgSlug } = params;
        const { brand_name, primary_color, secondary_color, logo } = body as any;

        // Get organization ID
        const org = await db.fetchOne<Organization>(
          'SELECT id FROM organizations WHERE slug = ?',
          [orgSlug]
        );

        if (!org) {
          return {
            success: false,
            error: 'Organization not found'
          };
        }

        // Upsert brand settings
        await db.execute(`
          INSERT INTO brand_settings 
            (organization_id, brand_name, primary_color, secondary_color, logo_data)
          VALUES (?, ?, ?, ?, ?)
          ON CONFLICT(organization_id) DO UPDATE SET
            brand_name = excluded.brand_name,
            primary_color = excluded.primary_color,
            secondary_color = excluded.secondary_color,
            logo_data = excluded.logo_data
        `, [org.id, brand_name, primary_color, secondary_color, logo]);

        return {
          success: true
        };
      } catch (error) {
        logger.error('Error saving brand settings', error);
        return {
          success: false,
          error: 'Failed to save brand settings'
        };
      }
    });
} 