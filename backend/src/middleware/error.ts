import { Elysia } from 'elysia'
import { logger } from '../logger'

export const errorHandler = new Elysia()
  .onError(({ code, error, set }) => {
    logger.error(`❌ Error: ${code} - ${error.message}`);
    logger.error(`Stack trace: ${error.stack}`);
    
    switch (code) {
      case 'NOT_FOUND':
        set.status = 404;
        return {
          success: false,
          message: 'Resource not found'
        };
      
      case 'VALIDATION':
        set.status = 400;
        return {
          success: false,
          message: error.message
        };
      
      default:
        set.status = 500;
        return {
          success: false,
          message: 'Internal server error'
        };
    }
  }); 