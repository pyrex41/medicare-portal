import { defineConfig } from 'vite'
import elmPlugin from 'vite-plugin-elm'
import path from 'path'

export default defineConfig({
  plugins: [elmPlugin()],
  css: {
    postcss: {
      plugins: [
        (await import('tailwindcss')).default,
        (await import('autoprefixer')).default,
      ],
    },
  },
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
    }
  },
  build: {
    outDir: '../dist',
    emptyOutDir: true,
    assetsDir: 'assets',
    rollupOptions: {
      input: {
        main: path.resolve(__dirname, 'index.html')
      }
    }
  }
})