import { Request, Response, NextFunction } from 'express';
import jwt from 'jsonwebtoken';
import { Database } from '../database';

interface AuthenticatedRequest extends Request {
  user?: {
    id: number;
    email: string;
    organization_id: number;
    role: string;
    turso_db_url?: string;
    turso_auth_token?: string;
  };
}

export async function authMiddleware(
  req: AuthenticatedRequest, 
  res: Response, 
  next: NextFunction
) {
  try {
    // Verify JWT token
    const token = req.headers.authorization?.split(' ')[1];
    if (!token) {
      res.status(401).json({ error: 'No token provided' });
      return;
    }

    const decoded = jwt.verify(token, process.env.JWT_SECRET!) as {
      userId: number;
    };

    const db = new Database();

    // Get user with organization info from central DB
    const user = await db.execute(
      `SELECT u.*, o.turso_db_url, o.turso_auth_token
       FROM users u
       JOIN organizations o ON u.organization_id = o.id
       WHERE u.id = ?`,
      [decoded.userId]
    );

    if (!user) {
      res.status(401).json({ error: 'User not found' });
      return;
    }

    req.user = user;
    next();
  } catch (error) {
    res.status(401).json({ error: 'Unauthorized' });
  }
} 