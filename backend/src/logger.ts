export const logger = {
  info: (message: string) => {
    const timestamp = new Date().toISOString()
    console.log(`[${timestamp}] [INFO] ${message}`)
  },
  error: (message: string) => {
    const timestamp = new Date().toISOString()
    console.error(`[${timestamp}] [ERROR] ${message}`)
  }
} 