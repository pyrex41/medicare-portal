/**
 * Adds animation to charts
 * This module contains functions to animate different chart types
 */

/**
 * Configure animations for line charts
 * @param chart - The Chartist.js line chart instance
 */
export function setupLineChartAnimations(chart: any) {
  if (!chart || typeof chart.on !== 'function') {
    console.error('Invalid chart instance provided to setupLineChartAnimations');
    return;
  }

  // Define a simpler animation approach
  chart.on('draw', function(data: any) {
    if (!data || !data.element) return;

    try {
      if (data.type === 'line') {
        // Simple fade in and slide for lines
        data.element.animate({
          opacity: {
            begin: 0,
            dur: 1000,
            from: 0,
            to: 1,
            easing: 'ease'
          }
        });
      } else if (data.type === 'point') {
        // Simple fade in for points with slight delay based on index
        data.element.animate({
          opacity: {
            begin: data.index * 80,
            dur: 500,
            from: 0,
            to: 1,
            easing: 'ease'
          }
        });
      }
    } catch (error) {
      console.error('Error in line chart animation:', error);
    }
  });
}

/**
 * Configure animations for bar charts
 * @param chart - The Chartist.js bar chart instance
 */
export function setupBarChartAnimations(chart: any) {
  if (!chart || typeof chart.on !== 'function') {
    console.error('Invalid chart instance provided to setupBarChartAnimations');
    return;
  }

  // Apply animations on draw
  chart.on('draw', function(data: any) {
    if (!data || !data.element) return;
    
    try {
      if (data.type === 'bar') {
        // Animate the bar with a simple grow from bottom effect
        data.element.animate({
          opacity: {
            begin: data.index * 80,
            dur: 500,
            from: 0,
            to: 1,
            easing: 'ease'
          },
          y1: {
            begin: data.index * 80,
            dur: 500,
            from: data.y2,
            to: data.y1,
            easing: 'ease'
          }
        });
      } else if (data.type === 'label') {
        // Fade in labels
        data.element.animate({
          opacity: {
            begin: 0,
            dur: 400,
            from: 0,
            to: 1
          }
        });
      }
    } catch (error) {
      console.error('Error in bar chart animation:', error);
    }
  });
}

/**
 * Configure animations for funnel charts
 * @param element - The DOM element containing the funnel chart
 */
export function animateFunnelChart(element: HTMLElement) {
  // Select all bars in the funnel chart
  const bars = element.querySelectorAll<HTMLElement>('.funnel-bar');
  const labels = element.querySelectorAll<HTMLElement>('.funnel-bar-label');
  const values = element.querySelectorAll<HTMLElement>('.funnel-bar-value');
  
  // Animate each bar with proper staggering
  bars.forEach((bar: HTMLElement, index: number) => {
    const targetWidth = bar.getAttribute('data-original-width') || '0%';
    
    setTimeout(() => {
      bar.style.transition = 'width 0.8s cubic-bezier(0.22, 0.61, 0.36, 1), opacity 0.6s ease';
      bar.style.width = targetWidth;
      bar.style.opacity = '1';
    }, 150 * index);
  });
  
  // Animate labels with a slight delay after bars
  labels.forEach((label: HTMLElement, index: number) => {
    label.style.opacity = '0';
    label.style.transform = 'translateY(10px)';
    
    setTimeout(() => {
      label.style.transition = 'opacity 0.5s ease, transform 0.5s ease';
      label.style.opacity = '1';
      label.style.transform = 'translateY(0)';
    }, 300 + 80 * index);
  });
  
  // Animate values with a slightly longer delay than labels
  values.forEach((value: HTMLElement, index: number) => {
    value.style.opacity = '0';
    value.style.transform = 'translateY(10px)';
    
    setTimeout(() => {
      value.style.transition = 'opacity 0.5s ease, transform 0.5s ease';
      value.style.opacity = '1';
      value.style.transform = 'translateY(0)';
    }, 450 + 80 * index);
  });
  
  // Add a subtle pulse animation to the largest bar (last one)
  setTimeout(() => {
    if (bars.length > 0) {
      const largestBar = bars[bars.length - 1];
      largestBar.style.animation = 'pulse 2s infinite';
      
      // Add keyframes for pulse animation if not already present
      if (!document.querySelector('#pulse-animation')) {
        const style = document.createElement('style');
        style.id = 'pulse-animation';
        style.textContent = `
          @keyframes pulse {
            0% { box-shadow: 0 0 0 0 rgba(3, 4, 94, 0.4); }
            70% { box-shadow: 0 0 0 6px rgba(3, 4, 94, 0); }
            100% { box-shadow: 0 0 0 0 rgba(3, 4, 94, 0); }
          }
        `;
        document.head.appendChild(style);
      }
    }
  }, 1500);
}