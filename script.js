// Theme Toggle
const themeToggle = document.getElementById('theme-toggle');
const html = document.documentElement;
const sunIcon = themeToggle.querySelector('.sun-icon');
const moonIcon = themeToggle.querySelector('.moon-icon');

const savedTheme = localStorage.getItem('theme') || 'light';
html.classList.toggle('dark', savedTheme === 'dark');
updateThemeIcons();

themeToggle.addEventListener('click', () => {
  const isDark = html.classList.toggle('dark');
  localStorage.setItem('theme', isDark ? 'dark' : 'light');
  updateThemeIcons();
});

function updateThemeIcons() {
  if (html.classList.contains('dark')) {
    sunIcon.style.display = 'block';
    moonIcon.style.display = 'none';
  } else {
    sunIcon.style.display = 'none';
    moonIcon.style.display = 'block';
  }
}

// Navbar Scroll Effect
const navbar = document.getElementById('navbar');
let lastScrollY = 0;

window.addEventListener('scroll', () => {
  lastScrollY = window.scrollY;
  navbar.classList.toggle('scrolled', lastScrollY > 20);
});

// Mobile Menu Toggle
const mobileMenuBtn = document.getElementById('mobile-menu-btn');
const mobileMenu = document.getElementById('mobile-menu');

mobileMenuBtn.addEventListener('click', () => {
  mobileMenu.classList.toggle('active');
});

document.querySelectorAll('.mobile-nav-link').forEach(link => {
  link.addEventListener('click', () => {
    mobileMenu.classList.remove('active');
  });
});

// Carousel Infinity Scroll & Snapping
const track = document.getElementById('carousel-track');
const container = document.querySelector('.carousel-container');
const prevBtn = document.getElementById('carousel-prev');
const nextBtn = document.getElementById('carousel-next');

if (track && container) {
  const cards = Array.from(track.children);
  
  // Gandakan card lebih banyak (4x) biar scroll-nya terasa benar-benar infinite
  for (let i = 0; i < 4; i++) {
    cards.forEach(card => track.appendChild(card.cloneNode(true)));
  }

  // Fungsi buat ngitung jarak geser (Lebar Card + Gap 24px) secara live
  const getSnapDistance = () => track.children[0].offsetWidth + 24;

  // Scroll pakai wheel mouse
  container.addEventListener('wheel', (e) => {
    if (e.deltaY !== 0) {
      e.preventDefault();
      if (e.deltaY > 0) {
        container.scrollBy({ left: getSnapDistance(), behavior: 'smooth' });
      } else {
        container.scrollBy({ left: -getSnapDistance(), behavior: 'smooth' });
      }
    }
  }, { passive: false });

  // Tombol Prev & Next
  if (prevBtn && nextBtn) {
    nextBtn.addEventListener('click', () => {
      container.scrollBy({ left: getSnapDistance(), behavior: 'smooth' });
    });
    prevBtn.addEventListener('click', () => {
      container.scrollBy({ left: -getSnapDistance(), behavior: 'smooth' });
    });
  }
}

// Intersection Observer for Animations
const observerOptions = { threshold: 0.1, rootMargin: '0px 0px -100px 0px' };
const observer = new IntersectionObserver(entries => {
  entries.forEach(entry => {
    if (entry.isIntersecting) entry.target.classList.add('visible');
  });
}, observerOptions);

document.querySelectorAll('.fade-in-scroll').forEach(el => observer.observe(el));

// Contact Form Logic (Email vs WA)
const contactForm = document.getElementById('contact-form');

if (contactForm) {
  contactForm.addEventListener('submit', (e) => {
    e.preventDefault();

    const name = document.getElementById('name').value.trim();
    const contactMethod = document.getElementById('contact-method').value.trim();
    const message = document.getElementById('message').value.trim();

    // Deteksi apakah inputnya email (mengandung logo '@')
    const isEmail = contactMethod.includes('@');

    if (isEmail) {
      // LOGIC 1: Buka aplikasi Email
      const targetEmail = "naradevane@gmail.com"; // <-- Ganti kalau lu bikin email naradevane
      const subject = encodeURIComponent(`New Inquiry from ${name}`);
      const body = encodeURIComponent(`Name: ${name}\nEmail: ${contactMethod}\n\nMessage:\n${message}`);
      window.open(`mailto:${targetEmail}?subject=${subject}&body=${body}`);
    } else {
      // LOGIC 2: Lempar ke WhatsApp
      const phoneNumber = "6285163635115"; // <-- GANTI NOMER WA LU DI SINI
      const waText = encodeURIComponent(`Hello, my name is ${name}.\n\n*My Contact:* ${contactMethod}\n*Message:*\n${message}`);
      window.open(`https://wa.me/${phoneNumber}?text=${waText}`, '_blank');
    }

    contactForm.reset();
    
    const formSuccess = document.getElementById('form-success');
    formSuccess.style.display = 'block';
    setTimeout(() => { formSuccess.style.display = 'none'; }, 5000);
  });
}