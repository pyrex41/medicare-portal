import { Router } from 'express';
import { db } from '../src/db';
import { z } from 'zod';

const router = Router();

// Schema for waitlist entry validation
const waitlistSchema = z.object({
    name: z.string().min(1, "Name is required"),
    email: z.string().email("Invalid email format"),
    phone: z.string().regex(/^\d{10}$/, "Phone number must be 10 digits"),
    numAgents: z.string().regex(/^\d+$/).transform(Number),
    bookSize: z.string().regex(/^\d+$/).transform(Number)
});

// POST /api/waitlist - Add new waitlist entry
router.post('/', async (req, res) => {
    try {
        // Validate request body
        const data = waitlistSchema.parse(req.body);

        // Check if email already exists
        const existing = await db.prepare(
            'SELECT id FROM waitlist WHERE email = ?'
        ).get(data.email);

        if (existing) {
            return res.status(400).json({
                success: false,
                message: 'Email already registered'
            });
        }

        // Insert new waitlist entry
        await db.prepare(`
            INSERT INTO waitlist (name, email, phone, num_agents, book_size)
            VALUES (?, ?, ?, ?, ?)
        `).run(
            data.name,
            data.email,
            data.phone,
            data.numAgents,
            data.bookSize
        );

        res.status(200).json({
            success: true,
            message: 'Successfully joined waitlist'
        });
    } catch (error) {
        console.error('Waitlist error:', error);
        
        if (error instanceof z.ZodError) {
            return res.status(400).json({
                success: false,
                message: error.errors[0].message
            });
        }

        res.status(500).json({
            success: false,
            message: 'Internal server error'
        });
    }
});

export default router; 