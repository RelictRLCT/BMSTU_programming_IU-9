#include <iostream>
#include <vector>
#include "GL/freeglut.h"
#include "GL/gl.h"
#include "GLFW/glfw3.h"
#include "cmath"
using namespace std;
int window_height = 840;
int window_width = 840;
bool ctrl_pressed = false;
bool smooth = false;

GLubyte* pixel_matrix = new GLubyte[window_width * window_height * 3];

typedef struct point_t {
    int x;
    int y;
};

vector<point_t> points(0);

typedef struct rebro_t {
    int x1;
    int y1;
    int x2;
    int y2;
};

void setPixel(int koordx, int koordy) {
    pixel_matrix[(koordx + window_width*koordy)*3] = 0;
    pixel_matrix[(koordx + window_width*koordy)*3+1] = 255;
    pixel_matrix[(koordx + window_width*koordy)*3+2] = 100;
}

void inverte_pixel(int koordx, int koordy) {
    for (int i = 0; i < window_width - koordx; i++) {
        if (pixel_matrix[(koordx + window_width*koordy + i)*3] == 0) {
            pixel_matrix[(koordx + window_width*koordy + i)*3] = 26;
            pixel_matrix[(koordx + window_width*koordy + i)*3 + 1] = 51;
            pixel_matrix[(koordx + window_width*koordy + i)*3 + 2] = 102;
        } else {
            pixel_matrix[(koordx + window_width*koordy + i)*3] = 0;
            pixel_matrix[(koordx + window_width*koordy + i)*3 + 1] = 255;
            pixel_matrix[(koordx + window_width*koordy + i)*3 + 2] = 100;
        }
    }
}

void ViewLine(rebro_t rebro) {
    const int deltaX = abs(rebro.x2 - rebro.x1);
    const int deltaY = abs(rebro.y2 - rebro.y1);
    const int signX = rebro.x1 < rebro.x2 ? 1 : -1;
    const int signY = rebro.y1 < rebro.y2 ? 1 : -1;
    int error = deltaX - deltaY;
    setPixel(rebro.x2, rebro.y2);
    while(rebro.x1 != rebro.x2 || rebro.y1 != rebro.y2) {
        setPixel(rebro.x1, rebro.y1);
        int error2 = error * 2;
        if(error2 > -deltaY) {
            error -= deltaY;
            rebro.x1 += signX;
        }
        if(error2 < deltaX) {
            error += deltaX;
            rebro.y1 += signY;
        }
    }
}

void rastr_image() {

    int size_points = points.size();

    for (int i = 1; i < size_points; i++) {

        rebro_t rebro = {points[i - 1].x, points[i - 1].y, points[i].x, points[i].y};

        const int deltaX = abs(rebro.x2 - rebro.x1);
        const int deltaY = abs(rebro.y2 - rebro.y1);
        const int signX = rebro.x1 < rebro.x2 ? 1 : -1;
        const int signY = rebro.y1 < rebro.y2 ? 1 : -1;
        int error = deltaX - deltaY;

        while (rebro.x1 != rebro.x2 || rebro.y1 != rebro.y2) {

            int error2 = error * 2;

            if(error2 < deltaX and points[i].x >= points[i - 1].x) {
                inverte_pixel(rebro.x1 + 1, rebro.y1);
            } else if (error2 < deltaX and points[i].x <= points[i - 1].x){
                inverte_pixel(rebro.x1 + 1, rebro.y1);
            }

            if(error2 > -deltaY) {
                error -= deltaY;
                rebro.x1 += signX;
            }
            if(error2 < deltaX) {
                error += deltaX;
                rebro.y1 += signY;
            }

        }
    }

    for (int i = 1; i < size_points; i++) {
        if (pixel_matrix[(window_width - 5 + window_width*points[i-1].y)*3] != pixel_matrix[(window_width - 5 + window_width*(points[i-1].y+1))*3]
        and pixel_matrix[(window_width - 5 + window_width*points[i-1].y)*3] != pixel_matrix[(window_width - 5 + window_width*(points[i-1].y-1))*3]) {
            inverte_pixel(points[i-1].x+1, points[i-1].y );
        }
    }
}

void smooth_image() {

    int size_points = points.size();

    for (int i = 1; i < size_points; i++) {

        rebro_t rebro = {points[i - 1].x, points[i - 1].y, points[i].x, points[i].y};

        const int deltaX = abs(rebro.x2 - rebro.x1);
        const int deltaY = abs(rebro.y2 - rebro.y1);
        const int signX = rebro.x1 < rebro.x2 ? 1 : -1;
        const int signY = rebro.y1 < rebro.y2 ? 1 : -1;

        int error = deltaX - deltaY;

        float m = float(deltaX)/deltaY;
        float e = 1/2;
        float w = 1-m;

        while (rebro.x1 != rebro.x2 || rebro.y1 != rebro.y2) {

            pixel_matrix[(rebro.x1 + window_width*rebro.y1)*3] = 0;
            pixel_matrix[(rebro.x1 + window_width*rebro.y1)*3+1] = 255 * (e + w);
            pixel_matrix[(rebro.x1 + window_width*rebro.y1)*3+2] = 100 * (e + w);

            int error2 = error * 2;

            if(error2 > -deltaY) {
                error -= deltaY;
                rebro.x1 += signX;
            }
            if(error2 < deltaX) {
                error += deltaX;
                rebro.y1 += signY;
            }

            if (e < w) {
                e += m;
            } else {
                e -= w;
            }
            //cout << e + w << endl;
        }
    }
}

void pixel_matrix_reset() {
    for (int i = 0; i < window_width * window_height * 3; i += 3) {
        pixel_matrix[i] = 26;
        pixel_matrix[i+1] = 51;
        pixel_matrix[i+2] = 102;
    }
}

void display(GLFWwindow *window) {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glEnable(GL_DEPTH_TEST);
    glLoadIdentity();
    pixel_matrix_reset();

    int size_points = points.size();
    if (size_points > 1) {
        rebro_t rebro;
        for (int i = 1; i < size_points; i++) {
            rebro.x1 = points[i-1].x;
            rebro.y1 = points[i-1].y;
            rebro.x2 = points[i].x;
            rebro.y2 = points[i].y;
            ViewLine(rebro);
        }
    } else if (size_points == 1) {
        setPixel(points[0].x, points[0].y);
    }

    if (ctrl_pressed) {
        rastr_image();
    }

    if (smooth) {
        smooth_image();
    }

    glRasterPos2f(-1, -1);
    glDrawPixels(window_width, window_height, GL_RGB, GL_UNSIGNED_BYTE, pixel_matrix);

    glFlush();
    glfwSwapBuffers(window);
    glfwPollEvents();
}


void CallbackKeys(GLFWwindow* window, int key, int scancode, int action, int mods) {

    if (action == GLFW_PRESS) {
        switch (key) {
            case GLFW_KEY_LEFT_CONTROL:
                if (ctrl_pressed) {
                    ctrl_pressed = false;
                    break;
                }
                if (points.size() > 0 and points[points.size()-1].x != points[0].x and points[points.size()-1].y != points[0].y) {
                    points.push_back(points[0]);
                }

                ctrl_pressed = true;
                break;
            case GLFW_KEY_LEFT_ALT:
                points.clear();
                ctrl_pressed = false;
                break;
            case GLFW_KEY_BACKSPACE:
                if (points.size() > 0) {
                    points.pop_back();
                }
                ctrl_pressed = false;
                break;
            case GLFW_KEY_S:
                smooth = !smooth;
                break;
        }
    }
}

void mouse(GLFWwindow* window, int button, int action, int mods) {

    if (button == GLFW_MOUSE_BUTTON_LEFT && action == GLFW_PRESS) {
        double xpos, ypos;
        glfwGetCursorPos(window, &xpos, &ypos);
        ypos = window_height - ypos;

        point_t point = {int(xpos), int(ypos)};
        points.push_back(point);
    }

}

int main() {

    glfwInit();
    GLFWwindow *window;

    window = glfwCreateWindow(window_width, window_height, "Laba 4", NULL, NULL);
    glfwMakeContextCurrent(window);

    glfwSetKeyCallback(window, CallbackKeys);
    glfwSetMouseButtonCallback(window, mouse);
    while (not glfwWindowShouldClose(window)) {
        display(window);
    }

    glfwDestroyWindow(window);
    glfwTerminate();
    return 0;
}
