import { defineConfig } from 'vite'
import elmPlugin from 'vite-plugin-elm'
import { resolve } from 'path'
import tailwindcss from '@tailwindcss/vite'

export default defineConfig({
  plugins: [
    elmPlugin({
      // Set a custom temp directory inside your project
      cwd: resolve(__dirname, 'elm-temp')
    }),
    tailwindcss(),
  ],
  resolve: {
    extensions: ['.ts', '.js', '.elm']
  },
  server: {
    proxy: {
      '/api': {
        target: 'http://localhost:8000',
        changeOrigin: true,
        secure: false,
        configure: (proxy, _options) => {
          proxy.on('error', (err) => {
            console.log('proxy error', err);
          });
          proxy.on('proxyReq', (proxyReq) => {
            console.log('Sending Request:', proxyReq.method, proxyReq.path);
          });
          proxy.on('proxyRes', (proxyRes) => {
            console.log('Received Response:', proxyRes.statusCode);
          });
        }
      }
    },
    allowedHosts: ['localhost', '127.0.0.1', 'medicaremax.ngrok.dev']
  },
  build: {
    outDir: '../dist',
    emptyOutDir: true,
    assetsDir: 'assets',
    rollupOptions: {
      input: {
        main: resolve(__dirname, 'index.html')
      }
    }
  }
})